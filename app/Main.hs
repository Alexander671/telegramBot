{-# LANGUAGE OverloadedStrings #-}
module Main where

import Types
import Network.HTTP.Simple (getResponseBody, httpBS, Request)
import Network.HTTP.Client (parseRequest,parseRequest_,Request(method, requestBody, requestHeaders),Response,RequestBody(RequestBodyBS) )
{------------------------------------}
import Data.Aeson ( decode )  
{------------------------------------}
import System.IO ( hClose, openFile, hGetContents, IOMode(ReadWriteMode) )
import           GHC.Generics             hiding (from )
{------------------------------------}
import Control.Monad ( join )
{------------------------------------}
import qualified Data.ByteString.Lazy     as B
import qualified Data.ByteString.Char8    as BS


{--------------------------------------------}
{-customization-}
timeoutGetUpdates :: Integer
timeoutGetUpdates = 15

token = "1283054130:AAG9Wy-CNmZ_7V_Fv4AUFUotGwOLFEprM1E"

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
{--------------------------------------------}


main :: IO ()
main = do
    print "Take Updates from telegram:"
    updates <- getUpdates
    print updates
    
    response <- responseMessage $ url $ parseUpdate $ B.fromStrict updates 
    print "Response:"
    print response
    main

getUpdates :: IO BS.ByteString
getUpdates  = do
            res1 <- parseRequest (prefix ++ "getUpdates?timeout="++show timeoutGetUpdates)
            res  <- httpBS res1
            return $ getResponseBody res


parseUpdate :: B.ByteString -> MESSAGE
parseUpdate str =   
            (CONTACT    contactFile )                        <|>
            (TEXT       textMessage)                      <|>
            (VIDEO    $ textFile video file_id_video)     <|>
            (AUDIO    $ textFile audio file_id_audio)     <|>
            (VOICE    $ textFile voice file_id_voice)     <|>
            (DOCUMENT $ textFile document file_id_doc)    <|>
            (CALLBACK $ callbackFile)                         <|>
            (LOCATION $ locatFile)                        <|>
            (STICKER  $ textFile sticker file_id_sticker) <|>
            (OTHERMES $ other)                            <|>
            (NOINPUT)
                  where other = (getRes update_id,join $ getChat <$> (join $ getRes message))
                        textMessage =
                                    ((getRes update_id)
                                    ,(join $ join $ fmap getChat <$> getRes message)
                                    ,(join $ join $ fmap text <$> getRes message))
                        locatFile = ((getRes update_id)
                                    ,(join $ join $ fmap getChat <$> getRes message)
                                    ,(fmap latitude    $ join $ join $ fmap location <$> getRes message)
                                    ,(fmap longitude   $ join $ join $ fmap location <$> getRes message))
                        textFile file id_file =
                                    ((getRes update_id)
                                    ,(join $ join $ fmap getChat <$> getRes message)
                                    ,(fmap id_file $ join $ join $ fmap file <$> getRes message))
                        callbackFile  =
                                    ((getRes update_id)
                                    ,(fmap from_id $ join $ fmap from_callback <$> getRes callback_query)
                                    ,(fmap data_callback $ join $ getRes callback_query))
                        contactFile =
                              ((getRes update_id)
                              ,(join $ join $ fmap getChat <$> getRes message)
                              ,(fmap phone_number $ join $ join $ fmap contact <$> getRes message)
                              ,(fmap first_name   $ join $ join $ fmap contact <$> getRes message)
                              ,(join $ fmap user_id $ join $ join $ fmap contact <$> getRes message))
                        getRes f = do
                              res    <- decode str 
                              (r:rs) <- result res
                              return $ f r
                        getChat res = do
                              chat' <- chat res 
                              return $ chat_message_id chat'                      


delCallBack :: (Int,Int, Int, String) -> IO BS.ByteString
delCallBack x@(repeat,upd,cht,delUrl) = do
                musor <- changeConfig x
                updates <- getUpdates
                deleteUpdate delUrl 
                return updates

-- (ID обновления, выбранное кол-во повторов, ID пользователя)
changeConfig ::(Int,Int, Int, String)  -> IO ()
changeConfig (upd,repeat,cht,delUrl) = do
                handle <- openFile "configRepeat.log" ReadWriteMode
                contents <- hGetContents handle 
                seq (length contents) (return ())
                writeFile "configRepeat.log" $ show $ (upd,repeat,cht) : (read contents :: [(Int,Int,Int)])
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

url :: MESSAGE -> MESSAGEorCALL
url (TEXT (Just idUpdate,Just idFrom,Just m))
                     | m == "/repeat" = REPEAT $ ("{\"method\":\"sendMessage\", \"chat_id\" : " ++ show idFrom ++ ",\"text\" : \"", "\", \"reply_markup\" : {\"inline_keyboard\" : [[{\"text\":\"1\", \"callback_data\":\"1\"},{\"text\":\"2\", \"callback_data\":\"2\"},{\"text\":\"3\", \"callback_data\":\"3\"},{\"text\":\"4\", \"callback_data\":\"4\"},{\"text\":\"5\", \"callback_data\":\"5\"}]]}",delUrl idUpdate,idFrom)
                     | m == "/help"   = MESSAGE (prefix ++ "sendMessage?chat_id=" ++ show idFrom ++ "&text=" ++ textHelp,delUrl idUpdate,idFrom)
                     | otherwise      = MESSAGE (prefix ++ "sendMessage?chat_id=" ++ show idFrom ++ "&text=" ++ m,delUrl idUpdate,idFrom)
url (CONTACT (Just idUpdate,Just idFrom, Just phone, Just firstName,Just user)) =MESSAGE (prefix ++ "sendContact?chat_id=" ++ show idFrom ++ "&phone_number=" ++ phone ++ "&first_name=" ++ firstName,delUrl idUpdate,idFrom)
url (VIDEO   (Just idUpdate,Just idFrom, Just dataFile)) = MESSAGE (prefix ++ "sendVideo?chat_id=" ++ show idFrom ++ "&video=" ++ dataFile,delUrl idUpdate,idFrom)
url (AUDIO   (Just idUpdate,Just idFrom, Just dataFile)) = MESSAGE (prefix ++ "sendAudio?chat_id=" ++ show idFrom ++ "&audio=" ++ dataFile,delUrl idUpdate,idFrom)
url (STICKER (Just idUpdate,Just idFrom, Just dataFile)) = MESSAGE (prefix ++ "sendSticker?chat_id=" ++ show idFrom ++ "&sticker="    ++ dataFile,delUrl idUpdate,idFrom)
url (DOCUMENT(Just idUpdate,Just idFrom, Just dataFile)) = MESSAGE (prefix ++ "sendDocument?chat_id=" ++ show idFrom ++ "&document=" ++ dataFile,delUrl idUpdate,idFrom)
url (VOICE   (Just idUpdate,Just idFrom, Just dataFile)) = MESSAGE (prefix ++ "sendVoice?chat_id=" ++ show idFrom ++ "&voice="    ++ dataFile,delUrl idUpdate,idFrom)     
url (CALLBACK(Just idUpdate,Just idFrom, Just dataFile)) = CALL    (idUpdate,read dataFile::Int,idFrom,delUrl idUpdate)
url (OTHERMES(Just idUpdate,Just idFrom))                = MESSAGE (prefix ++ "sendMessage?chat_id=" ++ show idFrom ++ "&text="   ++ textNotFound,delUrl idUpdate,idFrom)
url (LOCATION(Just idUpdate,Just idFrom,Just lat,Just long)) = MESSAGE (prefix ++ "sendLocation?chat_id=" ++ show idFrom ++ "&latitude=" ++ show lat ++ "&longitude=" ++show long,delUrl idUpdate,idFrom)
url (NOINPUT)                                            = MESSAGE ("NOINPUT","",0)

delUrl :: (Show a, Num a) => a -> [Char]
delUrl idUpdate = prefix ++ "getUpdates?offset=" ++ (show $ idUpdate + 1)

responseMessage :: MESSAGEorCALL -> IO BS.ByteString
responseMessage (MESSAGE ("NOINPUT",_,_))=return "no update"
responseMessage (MESSAGE (req,del,cht))  = do
            res1 <- responseMessage2 (takeConfig cht) req 
            del' <- parseRequest del 
            httpBS del'
            return (getResponseBody res1)
responseMessage (CALL x) = delCallBack x
responseMessage (REPEAT x) = repeatMessage x

responseMessage2 :: IO Int -> String -> IO (Response BS.ByteString)
responseMessage2 rpts rqst = do
           res   <- rpts
           rqst' <- parseRequest rqst 
           res2  <- httpBS rqst'
           if res <= 1 then return res2 else responseMessage2 (return (res-1)) rqst

deleteUpdate :: String -> IO BS.ByteString
deleteUpdate del = do
      del' <-parseRequest del 
      response <- httpBS del'
      return $ getResponseBody response

repeatMessage :: (String,String,String,Int) -> IO BS.ByteString
repeatMessage (part1,part2,del,cht) = do
                request' <- parseRequest prefix
                rpts <- takeConfig cht
                let request = request'
                        { method         = "POST"
                        , requestBody    = RequestBodyBS $ BS.pack (part1 ++ textRepeat rpts ++part2)  
                        , requestHeaders = [ ("Content-Type", "application/json; charset=utf-8")]
                        }   
                response <- httpBS request
                deleteUpdate del              
                return $ getResponseBody response