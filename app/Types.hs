{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Types where

import Data.Aeson
import GHC.Generics

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
    , video :: Maybe TelegramVideo
    } deriving (Show,Generic)

data TelegramVideo = TelegramVideo
   { file_id :: String
   } deriving (Show,Generic)

instance FromJSON (TelegramVideo) where
        parseJSON (Object v) = TelegramVideo <$>
                                 v .: "file_id"
        parseJSON _          = fail "not TelegramVideo"
instance ToJSON (TelegramVideo) where 

instance FromJSON (TelegramMessage) where
        parseJSON (Object v) = TelegramMessage <$>
                                 v .: "message_id" <*>
                                 v .:? "chat" <*>
                                 v .:? "text" <*>
                                 v .:? "reply_markup" <*>
                                 v .:? "video"
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