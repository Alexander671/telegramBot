{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main where

import           Network.HTTP.Simple            ( httpBS, getResponseBody,parseRequestThrow_,Request )
import           Data.Aeson  
import           Data.Text                hiding (zip,map)
import           Data.Aeson.Types          
import           Control.Monad
import           GHC.Generics             hiding (from )
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8         as BS
import Lib

token :: String
token = undefined

fstTuple  (a,_,_) = a
sndTuple  (_,a,_) = a
thirdTuple  (_,_,a) = a
 

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
    } deriving (Show,Generic)

instance FromJSON (TelegramMessage) where
        parseJSON (Object v) = TelegramMessage <$>
                                 v .: "message_id" <*>
                                 v .:? "from" <*>
                                 v .:? "text"
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
            res <- httpBS url
            return (getResponseBody res)
        where url =parseRequestThrow_ ("https://api.telegram.org/bot" ++ token ++ "/getUpdates?timeout=10")

           
getMessage :: B.ByteString -> Maybe [(Int, Maybe (Maybe Int),Maybe (Maybe String))]
getMessage id1 = do
            res         <- decode id1 :: Maybe TelegramUpdate   
            resResult   <- result res
            return $ zip3 (fmap update_id resResult) (fmap (fmap (fmap chat_message_id)) (fmap (fmap chat) (mess resResult))) $ fmap (fmap text) $ mess resResult
            where mess resResult = (fmap message resResult)

putMessage :: Maybe [(Int, Maybe (Maybe Int),Maybe (Maybe String))] -> [IO BS.ByteString]
putMessage mes =  requestMessage urlUpdate_id $ url urlMessage urlChatId
                  where urlUpdate_id = fmap (map fstTuple) mes
                        urlMessage   = fmap (map thirdTuple) mes
                        urlChatId    = fmap  (map sndTuple) mes 

url :: Maybe [Maybe (Maybe [Char])] -> Maybe [Maybe (Maybe Int)] -> [Maybe Request]
url (Just []) _  = []
url  _ (Just []) = []
url  _   Nothing = Nothing : []
url Nothing  _   = Nothing : []
url (Just ((Just (Just m)):mc)) (Just ((Just (Just c)):chat)) =Just (parseRequestThrow_ ("https://api.telegram.org/bot" ++ token ++ "/sendMessage?chat_id=" ++ (show c) ++ "&text="++ m )) : url (Just mc) (Just chat)


requestMessage :: Maybe [Int] -> [Maybe Request] -> [IO BS.ByteString]
requestMessage (Just [])         _      = []
requestMessage (Just (x:xs)) (Nothing:req)  = requestMessage (Just xs) req
requestMessage (Just (x:xs)) ((Just r):req) = (do
                         res  <- httpBS r
                         res2 <- httpBS (parseReq x)
                         return (getResponseBody res2)) : requestMessage (Just xs) req
                        where parseReq x = parseRequestThrow_ $ "https://api.telegram.org/bot" ++ token ++ "/getUpdates?offset=" ++ show (x+1) ++ "&timeout=10"
