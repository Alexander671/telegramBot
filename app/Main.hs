{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main where

import           Network.HTTP.Simple
import           System.IO
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Data.Aeson  
import           Data.Text                hiding (zip,map,filter,head,length)
import           Data.Aeson.Types          
import           Control.Monad
import           GHC.Generics             hiding (from )
import qualified Data.ByteString.Lazy     as B
import qualified Data.ByteString.Char8    as BS
import Lib

token :: String
token = "https://api.telegram.org/bot"

textRepeat :: Int -> String
textRepeat cht = "Your amount of repeats is " ++ show cht ++ ". Choose amount of repeats"

textHelp :: String
textHelp = "Hello, it's echo telegram bot."

amountOfStartRepeat = 1

fstTuple  (a,_,_) = a
sndTuple  (_,a,_) = a
thirdTuple  (_,_,a) = a


data TelegramKeyBoard = TelegramKeyBoard
     { inline_keyboard :: [[String]]
     
     } deriving (Show,Generic)

instance FromJSON TelegramKeyBoard where
        parseJSON (Object v) = TelegramKeyBoard <$>
                              v .: "inline_keyboard"
        parseJSON _          = fail "not TelegramKeyBoard"
instance ToJSON TelegramKeyBoard where


data TelegramChat = TelegramChat
    { chat_message_id :: Int
    
    } deriving (Show,Generic)

instance FromJSON (TelegramChat) where
        parseJSON (Object v) = TelegramChat <$>
                                 v .: "id" 
        parseJSON _          = fail "not TelegramFrom"
instance ToJSON (TelegramChat) where


data TelegramMessage= TelegramMessage
    { message_id :: Int
    , chat :: Maybe TelegramChat
    , text :: Maybe String
    , reply_markup :: Maybe TelegramKeyBoard
    } deriving (Show,Generic)

instance FromJSON (TelegramMessage) where
        parseJSON (Object v) = TelegramMessage <$>
                                 v .: "message_id" <*>
                                 v .:? "chat" <*>
                                 v .:? "text" <*>
                                 v .:? "reply_markup"
        parseJSON _          = fail "not TelegramMessage"
instance ToJSON (TelegramMessage) where 

data TelegramUser = TelegramUser
  { from_id :: Int
  } deriving (Show,Generic)

instance FromJSON TelegramUser where
        parseJSON (Object v) = TelegramUser <$>
                             v .: "id"
instance ToJSON TelegramUser where

data TelegramCallBackQuery = TelegramCallBackQuery
  { id_callback :: String
  , from_callback :: TelegramUser
  , data_callback :: String
  } deriving (Show,Generic)

instance FromJSON TelegramCallBackQuery where
        parseJSON (Object v) = TelegramCallBackQuery <$>
                             v .: "id" <*>
                             v .: "from" <*>
                             v .: "data" 
instance ToJSON TelegramCallBackQuery where

data TelegramResult = TelegramResult
  { update_id :: Int
  , message :: Maybe TelegramMessage
  , callback_query :: Maybe TelegramCallBackQuery
  } deriving (Show,Generic)

instance FromJSON TelegramResult where
        parseJSON (Object v) = TelegramResult <$>
                                 v .: "update_id" <*>
                                 v .:? "message" <*>
                                 v .:? "callback_query"
        parseJSON _          = fail "not TelegramFrom"
instance ToJSON TelegramResult where


data TelegramUpdate = TelegramUpdate
  { ok :: Bool
  , result :: Maybe [TelegramResult]
  , description :: Maybe String
  } deriving (Show,Generic)


instance FromJSON (TelegramUpdate) where
        parseJSON (Object v) = TelegramUpdate <$>
                                 v .: "ok" <*>
                                 v .:? "result" <*>
                                 v .:? "decription" 
        parseJSON _          = fail "not TelegramUp"
instance ToJSON (TelegramUpdate) where


main :: IO ()
main = do
    updates <- getUpdates
    res <- sequenceA $ getCallBackOrMessage $ B.fromStrict updates 
    print res
    main

getUpdates :: IO BS.ByteString
getUpdates  = do
            res1 <- parseRequest (token ++ "getUpdates?timeout=15")
            res  <- httpBS res1
            return $ getResponseBody res


getCallBackOrMessage str = case (check str) of
                           Nothing -> putMessage $ getMessage str
                           Just x ->  [delCallBack str x]
                        where check str = do 
                                        res       <- decode str  
                                        (r:resResult) <- result res
                                        cb <- callback_query r
                                        return cb
               

delCallBack :: B.ByteString ->  TelegramCallBackQuery -> IO (BS.ByteString)
delCallBack str x = do
                musor <- changeConfig $ getCallBack x $ id1 str
                updates <- getUpdates
                req <- httpBS (parseRequest_ $ deleteUpdate $ fstTuple $ getCallBack x $ id1 str)
                return updates
              where id1 str =  do
                        res <- decode str
                        (r:resResult) <- result res
                        return $ update_id r

getCallBack :: TelegramCallBackQuery -> Maybe Int -> (Int,Int,Int)
getCallBack str (Just x) = createField str
                where createField cb = (,,) x (read (data_callback cb) :: Int) $ from_id $ from_callback cb

-- (ID обновления, выбранное кол-во повторов, ID пользователя)
changeConfig :: (Int,Int,Int) -> IO ()
changeConfig cnfg = do
                handle <- openFile "configRepeat.txt" ReadWriteMode
                contents <- hGetContents handle 
                seq (length contents) (return ())
                writeFile "configRepeat.txt" $ show $ cnfg : (read contents :: [(Int,Int,Int)])
                hClose handle


takeConfig :: Int -> IO Int
takeConfig iD = do
                handle <- openFile "configRepeat.txt" ReadWriteMode
                contents <- hGetContents handle 
                seq (length contents) (return ())
                hClose handle
                return $ head1 $ filter (\(x,y,z) -> z==iD) (read contents :: [(Int,Int,Int)])
                               --filter (\(x,y,z) -> z==iD)
head1 [] = amountOfStartRepeat
head1 ((x,y,z):xs) = y 

getMessage :: B.ByteString -> Maybe [(Int, Maybe (Maybe Int),Maybe (Maybe String))]
getMessage id1 = do
            res         <- decode id1 :: Maybe TelegramUpdate   
            resResult   <- result res
            return $ zip3 (fmap update_id resResult) 
                          (chatid resResult)
                          $ fmap (text <$>) $ mess resResult           
            where mess resResult = message <$> resResult
                  chatid resResult = ((chat_message_id <$>) <$>) <$> (chat <$>) <$> mess resResult


putMessage :: Maybe [(Int, Maybe (Maybe Int),Maybe (Maybe String))] -> [IO BS.ByteString]
putMessage mess =  url urlUpdate_id urlMessage urlChatId
                  where urlUpdate_id = (map fstTuple) <$> mess
                        urlMessage   = (map thirdTuple) <$> mess
                        urlChatId    = (map sndTuple) <$> mess


url :: Maybe [Int] -> Maybe [Maybe (Maybe [Char])] -> Maybe [Maybe (Maybe Int)] -> [IO BS.ByteString]
url (Just []) _ _ = []
url _ (Just (Nothing:xs)) _ = []
url _ _ (Just [])  = []
url _ _ Nothing = []
url _ Nothing _ = []
url Nothing _ _ = []    
url (Just (x:xs)) (Just ((Just (Just m)):mc)) (Just ((Just (Just c)):chat)) 
                     | m == "/help"   = responseMessage (-5) x (parseRequest_ (token ++ "sendMessage?chat_id=" ++ (show c) ++ "&text=" ++ textHelp)) : url (Just xs) (Just mc) (Just chat) 
                     | m == "/repeat" = repeatMessage c x : url (Just xs) (Just mc) (Just chat)
                     | otherwise      = responseMessage c x (parseRequest_ (token ++ "sendMessage?chat_id=" ++ (show c) ++ "&text="++ m )) : url (Just xs) (Just mc) (Just chat)


deleteUpdate :: Int -> String
deleteUpdate x = token ++ "getUpdates?offset=" ++ show (x+1) 

responseMessage :: Int -> Int ->  Request -> IO BS.ByteString
responseMessage cht iD req = do
            res2 <- httpBS del 
            res1 <- responseMessage2 (takeConfig cht) req 
            return (getResponseBody res1)
            where del = parseRequest_ $ deleteUpdate iD 
          
responseMessage2 :: IO (Int) -> Request -> IO (Response BS.ByteString)
responseMessage2 rpts rqst = do
           res  <- rpts
           res2 <- httpBS rqst
           if (res <= 1) then return res2 else responseMessage2 (return (res-1)) rqst

repeatMessage :: Int -> Int -> IO BS.ByteString
repeatMessage cht iD = do
                request' <- parseRequest $ token
                rpts <- takeConfig cht
                let request = request'
                        { method         = "POST"
                        , requestBody    = RequestBodyBS $ BS.pack ("{\"method\":\"sendMessage\", \"chat_id\" : " ++ (show cht) ++ ",\"text\" : \"" ++ textRepeat rpts ++ "\", \"reply_markup\" : {\"inline_keyboard\" : [[{\"text\":\"1\", \"callback_data\":\"1\"},{\"text\":\"2\", \"callback_data\":\"2\"},{\"text\":\"3\", \"callback_data\":\"3\"},{\"text\":\"4\", \"callback_data\":\"4\"},{\"text\":\"5\", \"callback_data\":\"5\"}]]}")  
                        , requestHeaders = [ ("Content-Type", "application/json; charset=utf-8")]
                        }   
                response <- httpBS request
                musor <- httpBS del                
                return $ getResponseBody response
                where del = parseRequest_ $ deleteUpdate iD
                      manipulationWithList update contents = ((thirdTuple <$> head <$> getMessage update),(sndTuple <$> head <$> getMessage update)) : (read contents ::[(Maybe (Maybe (Maybe String)),Maybe (Maybe (Maybe Int)))])