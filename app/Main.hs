{-# LANGUAGE OverloadedStrings #-}

module Main where

import Types
import           Network.HTTP.Simple
import           System.IO
import           Network.HTTP.Client
import           Data.Aeson  
import           Control.Monad
import           GHC.Generics             hiding (from )
import qualified Data.ByteString.Lazy     as B
import qualified Data.ByteString.Char8    as BS


timeoutGetUpdates :: Integer
timeoutGetUpdates = 15

token = "1283054130:AAGAxsDcZAGYom8UEnqGqCFqmZ4_FuJG3mU"

prefix :: String
prefix = "https://api.telegram.org/bot" ++ token ++ "/"

textNotFound :: String
textNotFound = "This type of call is not supported"

textRepeat :: Int -> String
textRepeat cht = "Your amount of repeats is " ++ show cht ++ ". Choose amount of repeats"

textHelp :: String
textHelp = "Hello, it's echo telegram bot."

amountOfStartRepeat :: Int
amountOfStartRepeat = 1

main :: IO ()
main = do
    print "Take Updates from telegram:"
    updates <- getUpdates
    print updates
    response <- getCallBackOrMessage $ B.fromStrict updates 
    print "Response:"
    print response
    main

getUpdates :: IO BS.ByteString
getUpdates  = do
            res1 <- parseRequest (prefix ++ "getUpdates?timeout="++show timeoutGetUpdates)
            res  <- httpBS res1
            return $ getResponseBody res


getCallBackOrMessage :: B.ByteString -> IO BS.ByteString
getCallBackOrMessage str = case check str of
                              Just x ->  delCallBack str x
                              Nothing -> 
                                    case getVideo str of
                                          Just(x,Just y,Just z) -> urlVideo (x,Just y,Just z)
                                          _ ->  case getMessage str of 
                                                      Just (x, Just y, Just z) -> urlMessage $ Just (x, Just y, Just z)
                                                      _ -> urlOther $ getIdToDel str
                        where 
                              check str = do 
                                        res       <- decode str  
                                        (r:resResult) <- result res
                                        callback_query r
                              getIdToDel :: B.ByteString -> Maybe (Int, Int)
                              getIdToDel str = do
                                        res           <- decode str  
                                        (r:resResult) <- result res
                                        mess <- message r
                                        chat   <- chat mess
                                        return  (chat_message_id chat, update_id r)
               

delCallBack :: B.ByteString ->  TelegramCallBackQuery -> IO BS.ByteString
delCallBack str x = do
                musor <- changeConfig $ getCallBack x $ id1 str
                updates <- getUpdates
                deleteUpdate $ fstTuple $ getCallBack x $ id1 str
                return updates
              where id1 str =  do
                        res <- decode str
                        (r:resResult) <- result res
                        return $ update_id r
                    fstTuple  (a,_,_) = a

getCallBack :: TelegramCallBackQuery -> Maybe Int -> (Int,Int,Int)
getCallBack str (Just x) = createField str
                where createField cb = (,,) x (read (data_callback cb) :: Int) $ from_id $ from_callback cb

-- (ID обновления, выбранное кол-во повторов, ID пользователя)
changeConfig :: (Int,Int,Int) -> IO ()
changeConfig cnfg = do
                handle <- openFile "configRepeat.log" ReadWriteMode
                contents <- hGetContents handle 
                seq (length contents) (return ())
                writeFile "configRepeat.txt" $ show $ cnfg : (read contents :: [(Int,Int,Int)])
                hClose handle


takeConfig :: Int -> IO Int
takeConfig iD = do
                handle1 <- openFile "configRepeat.log" ReadWriteMode
                contents <- hGetContents handle1
                seq (length contents) (return ())
                hClose handle1
                return $ head1 $ filter (\(x,y,z) -> z==iD) (read contents :: [(Int,Int,Int)])

head1 :: [(a, Int, c)] -> Int
head1 [] = amountOfStartRepeat
head1 ((x,y,z):xs) = y 

getMessage :: B.ByteString -> Maybe (Int, Maybe Int,Maybe String)
getMessage id1 = do
            res             <- decode id1 :: Maybe TelegramUpdate   
            (r:resResult)   <- result res
            return $ (,,) (update_id r) 
                          (join $ chatid r)
                          (fmap join (text <$>) $ message r)           
            where chatid resResult = ((chat_message_id <$>) <$>) . fmap chat $ message resResult

getVideo :: B.ByteString -> Maybe (Int, Maybe Int, Maybe String)
getVideo id1 = do
            res           <- decode id1 :: Maybe TelegramUpdate   
            (r:resResult) <- result res
            return $ (,,)  
                  (update_id r) 
                  (join $ chatid r)
                  (fmap file_id =<< (video <$> message r))
            where chatid resResult = ((chat_message_id <$>) <$>) . fmap chat $ message resResult

urlVideo :: (Int, Maybe Int, Maybe String) ->  IO BS.ByteString
urlVideo (i,Just c,Just v) = responseMessage c i (parseRequest_ $ prefix ++  "sendVideo?chat_id=" ++ show c ++ "&video=" ++ v)

urlMessage :: Maybe (Int, Maybe Int, Maybe String) -> IO BS.ByteString
urlMessage (Just (x,Just c,Just m)) 
                     | m == "/repeat" = repeatMessage c x 
                     | m == "/help"   = responseMessage (-5) x (parseRequest_ (prefix ++ "sendMessage?chat_id=" ++ show c ++ "&text=" ++ textHelp)) 
                     | otherwise      = responseMessage c x (parseRequest_ (prefix ++ "sendMessage?chat_id=" ++ show c ++ "&text="++ m )) 


urlOther :: Maybe (Int, Int) -> IO BS.ByteString
urlOther Nothing = return "Can not parse update_id"
urlOther (Just (c,i)) = do 
            response <- httpBS $ parseRequest_ $ prefix ++ "sendMessage?chat_id=" ++ show c ++ "&text=" ++ textNotFound
            responseDel <- httpBS $ parseRequest_ $ del i
            return $ BS.pack textNotFound 
             where del x = prefix ++ "getUpdates?offset=" ++ show (x+1) 

deleteUpdate :: Int -> IO BS.ByteString
deleteUpdate x = do 
            response <- httpBS $ parseRequest_ $ del x
            return $ BS.pack $ show response 
             where del x = prefix ++ "getUpdates?offset=" ++ show (x+1) 


responseMessage :: Int -> Int ->  Request -> IO BS.ByteString
responseMessage cht iD req = do
            res1 <- responseMessage2 (takeConfig cht) req 
            deleteUpdate iD
            return (getResponseBody res1)
          
responseMessage2 :: IO Int -> Request -> IO (Response BS.ByteString)
responseMessage2 rpts rqst = do
           res  <- rpts
           res2 <- httpBS rqst
           if res <= 1 then return res2 else responseMessage2 (return (res-1)) rqst

repeatMessage :: Int -> Int -> IO BS.ByteString
repeatMessage cht iD = do
                request' <- parseRequest prefix
                rpts <- takeConfig cht
                let request = request'
                        { method         = "POST"
                        , requestBody    = RequestBodyBS $ BS.pack ("{\"method\":\"sendMessage\", \"chat_id\" : " ++ show cht ++ ",\"text\" : \"" ++ textRepeat rpts ++ "\", \"reply_markup\" : {\"inline_keyboard\" : [[{\"text\":\"1\", \"callback_data\":\"1\"},{\"text\":\"2\", \"callback_data\":\"2\"},{\"text\":\"3\", \"callback_data\":\"3\"},{\"text\":\"4\", \"callback_data\":\"4\"},{\"text\":\"5\", \"callback_data\":\"5\"}]]}")  
                        , requestHeaders = [ ("Content-Type", "application/json; charset=utf-8")]
                        }   
                response <- httpBS request
                deleteUpdate iD              
                return $ getResponseBody response
                