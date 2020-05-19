{-# LANGUAGE OverloadedStrings #-}
module Main where


import           Network.HTTP.Simple            ( httpBS, getResponseBody,parseRequestThrow_ )
import           Control.Lens                   ( preview )
import           Data.Aeson.Lens   
import           Data.Aeson   
import           Data.Aeson.Types          
import           Control.Monad
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8         as BS
import Lib



--------------------------------------------------
-- Первый уровень JSON                          --
--------------------------------------------------
data TelegramUpdate a = TelegramUpdate
  {   telegramUpdateOK :: Bool
    , telegramUpdateResult :: Maybe a
  } deriving Show

--------------------------------------------------
-- Представитель класса FromJSON                --
-- для использования функции decode             --
--------------------------------------------------
instance FromJSON (TelegramUpdate a)  where
        parseJSON (Object v) = TelegramUpdate <$>
                            v .: "ok" <*>
                            v .: "result"
        parseJSON  _         = fail "parseJSON error not objectType"


{--------------------------------------------------
-- Второй уровень JSON (result)                 --
--------------------------------------------------
data TelegramUpdateResult a = TelegramUpdateResult
  {   telegramUpdateOK :: Bool
    , telegramUpdateResult :: Maybe a
  } deriving Show


--                //--//--//                    --
instance FromJSON (TelegramUpdate a)  where
        parseJSON (Object v) = TelegramUpdate <$>
                            v .: "ok" <*>
                            v .: "result"
        parseJSON  _         = fail "parseJSON error not objectType"
-}


token :: String
token = "1283054130:AAFMfS1-WADlOtvj77jOm9COzDc8D-JLJIE"

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



getId id1 = do  
               result <- decode id1 :: Maybe (Object)
               flip parseMaybe result $ \obj -> do
                   res <- obj .: "result"         
                   return (res)
--995320169