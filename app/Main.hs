{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main where

import           Network.HTTP.Simple            ( httpBS, getResponseBody,parseRequestThrow_ )
import           Data.Aeson  
import           Data.Text                hiding (zip)
import           Data.Aeson.Types          
import           Control.Monad
import           GHC.Generics             hiding (from )
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8         as BS
import Lib

token :: String
token = "1283054130:AAFMfS1-WADlOtvj77jOm9COzDc8D-JLJIE"

data TelegramFrom = TelegramFrom
    { chat_message_id :: Int
    
    } deriving (Show)

instance FromJSON (TelegramFrom) where
        parseJSON (Object v) = TelegramFrom <$>
                                 v .: "id" 
        parseJSON _          = fail "not TelegramFrom"
instance ToJSON (TelegramFrom) where
        toJSON (TelegramFrom id) = object ["id" .= id]


data TelegramMessage= TelegramMessage
    { message_id :: Int
    , text :: String
    , from :: TelegramFrom
    } deriving (Show,Generic)

instance FromJSON (TelegramMessage) where
instance ToJSON (TelegramMessage) where 

data TelegramResult = TelegramResult
  { update_id :: Int
  , message :: TelegramMessage
  } deriving (Show,Generic)

instance FromJSON TelegramResult where
instance ToJSON TelegramResult where

data TelegramUpdate = TelegramUpdate
  { ok :: Bool
  , result :: Maybe [TelegramResult]
  , description :: Maybe String
  } deriving (Show,Generic)


instance FromJSON (TelegramUpdate) where
instance ToJSON (TelegramUpdate) where



main :: IO (Maybe [(Int,String)])
main = do
    updates <- getUpdates
    return (getMessage (B.fromStrict updates)) 


getUpdates :: IO BS.ByteString
getUpdates = do
            res <- httpBS url
            return (getResponseBody res)
        where url =parseRequestThrow_ ("https://api.telegram.org/bot" ++ token ++ "/getUpdates")

           

getMessage :: B.ByteString -> Maybe [(Int,String)]
getMessage id1 = do
            res         <- decode id1 :: Maybe TelegramUpdate
            resResult   <- (result res)
            resMessage  <- return (fmap message resResult)
            resFrom     <- return (fmap from resMessage)
            resIdChat   <- return (fmap chat_message_id resFrom)
            return $ zip resIdChat $ fmap text resMessage