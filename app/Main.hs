{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main where

import           Network.HTTP.Simple
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Data.Aeson  
import           Data.Text                hiding (zip,map)
import           Data.Aeson.Types          
import           Control.Monad
import           GHC.Generics             hiding (from )
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8         as BS
import Lib

token :: String
token = "https://api.telegram.org/bot1283054130:AAFMfS1-WADlOtvj77jOm9COzDc8D-JLJIE/"


fstTuple  (a,_,_) = a
sndTuple  (_,a,_) = a
thirdTuple  (_,_,a) = a

textRepeat :: String
textRepeat = "Chhose amount of repeats"

data TelegramKeyBoard = TelegramKeyBoard
     { keyboard :: [[String]]
     
     } deriving (Show,Generic)

instance FromJSON TelegramKeyBoard where
        parseJSON (Object v) = TelegramKeyBoard <$>
                              v .: "keyboard"
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
                                 v .:? "from" <*>
                                 v .:? "text" <*>
                                 v .:? "reply_markup"
        parseJSON _          = fail "not TelegramMessage"
instance ToJSON (TelegramMessage) where 

data TelegramResult = TelegramResult
  { update_id :: Int
  , message :: Maybe TelegramMessage
  } deriving (Show,Generic)

instance FromJSON TelegramResult where
        parseJSON (Object v) = TelegramResult <$>
                                 v .: "update_id" <*>
                                 v .:? "message"
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
        parseJSON _          = fail "not TelegramUpdate"
instance ToJSON (TelegramUpdate) where


main :: IO ()
main = do
    updates <- getUpdates
    res <- sequenceA $ putMessage $ getMessage $ B.fromStrict updates 
    print res
    main


getUpdates :: IO BS.ByteString
getUpdates  = do
            res1 <- url
            res  <- httpBS res1
            return $ getResponseBody res
        where url =parseRequest (token ++ "getUpdates?timeout=10")

           
getMessage :: B.ByteString -> Maybe [(Int, Maybe (Maybe Int),Maybe (Maybe String))]
getMessage id1 = do
            res         <- decode id1 :: Maybe TelegramUpdate   
            resResult   <- result res
            return $ zip3 (fmap update_id resResult) 
                          (chatid resResult)
                          $ fmap (text <$>) $ mess resResult
            
            where mess resResult = message <$> resResult
                  chatid resResult = (((chat_message_id <$>) <$>) <$> (chat <$>) <$> mess resResult)

putMessage :: Maybe [(Int, Maybe (Maybe Int),Maybe (Maybe String))] -> [IO BS.ByteString]
putMessage mess =  url urlUpdate_id urlMessage urlChatId
                  where urlUpdate_id = (map fstTuple) <$> mess
                        urlMessage   = (map thirdTuple) <$> mess
                        urlChatId    = (map sndTuple) <$> mess


url :: Maybe [Int] -> Maybe [Maybe (Maybe [Char])] -> Maybe [Maybe (Maybe Int)] -> [IO BS.ByteString]
url (Just [])       _            _  = []
url   _      Nothing        _  = []
url (Just (x:xs)) (Just ((Just (Just m)):mc)) (Just ((Just (Just c)):chat)) 
                     | m == "/help"   = responseMessage x (parseRequest_ (token ++ "sendMessage?chat_id=" ++ (show c) ++ "&text=Маленький помощник")) : url (Just xs) (Just mc) (Just chat) 
                     
                     | m == "/repeat" = repeatMessage x : url (Just xs) (Just mc) (Just chat)
                     | otherwise      = responseMessage x (parseRequest_ (token ++ "sendMessage?chat_id=" ++ (show c) ++ "&text="++ m )) : url (Just xs) (Just mc) (Just chat)


deleteUpdate :: Int -> String
deleteUpdate x = token ++ "getUpdates?offset=" ++ show (x+1) 


responseMessage :: Int ->  Request -> IO BS.ByteString
responseMessage iD  x = do
            res1  <- httpBS x
            res2  <- httpBS del
            return (getResponseBody res1)
            where del = parseRequest_ $ deleteUpdate iD
         
repeatMessage ::Int -> IO BS.ByteString
repeatMessage iD = do
                request' <- parseRequest $ token
                let request = request'
                        { method         = "POST"
                        , requestBody    = RequestBodyBS $ BS.pack ("{\"method\":\"sendMessage\", \"chat_id\" : " ++ (show 419088070) ++ ",\"text\" : \"" ++ textRepeat ++ "\",\"reply_markup\" : {\"keyboard\" : [[\"1\",\"2\",\"3\",\"4\",\"5\"]]}")  
                        , requestHeaders = [ ("Content-Type", "application/json; charset=utf-8")]
                        }   
                response <- httpBS request
                musor <- httpBS del
                return $ getResponseBody response
                where del = parseRequest_ $ deleteUpdate iD
          