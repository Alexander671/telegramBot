{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main where

import           Network.HTTP.Simple            ( httpBS, getResponseBody,parseRequestThrow_ )
import           Data.Aeson  
import           Data.Text 
import           Data.Aeson.Types          
import           Control.Monad
import           GHC.Generics
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8         as BS
import Lib

token :: String
token = "1283054130:AAFMfS1-WADlOtvj77jOm9COzDc8D-JLJIE"
 

data TelegramResult = TelegramResult
  { update_id :: Int
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



main :: IO ()
main = do
    updates <- getUpdates
    print  (getId (B.fromStrict updates))
    
{- 
delUpdate :: Maybe Int -> запрос на (update_id +1)
delUpdate id1 = do
            id2 <- id1
            return (id2)
-}               
                
getUpdates :: IO BS.ByteString
getUpdates = do
            res <- httpBS url
            return (getResponseBody res)
        where url =parseRequestThrow_ ("https://api.telegram.org/bot" ++ token ++ "/getUpdates")

getId :: B.ByteString -> Maybe [Int]
getId id1 = do
            res  <- decode id1 :: Maybe TelegramUpdate
            res2 <- (result res)
            return (fmap update_id res2)