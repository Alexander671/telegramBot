{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Types where

import Data.Aeson
    ( FromJSON(parseJSON), ToJSON, (.:), (.:?), Value(Object) )
import GHC.Generics ( Generic )


data MESSAGEorCALL = MESSAGE (String,String,Int) | CALL (Int,Int,Int,String) | -- (upd_id,repeats,cht_id,del)
                     REPEAT (String,String,String,Int)
data MESSAGE =
                CONTACT  (Maybe Int,Maybe Int, Maybe String,Maybe String,Maybe Integer)| -- (upd_id,cht_id,phone,first_name,user)
                TEXT     (Maybe Int,Maybe Int, Maybe String)|                            -- (upd_id,cht_id,content)
                VIDEO    (Maybe Int,Maybe Int, Maybe String)|                            -- // -//--
                AUDIO    (Maybe Int,Maybe Int, Maybe String)|                            -- // -//--
                VOICE    (Maybe Int,Maybe Int, Maybe String)|                            -- // -//--
                DOCUMENT (Maybe Int,Maybe Int, Maybe String)|                            -- // -//--
                STICKER  (Maybe Int,Maybe Int, Maybe String)|                            -- // -//--
                CALLBACK (Maybe Int,Maybe Int, Maybe String)|                            -- (upd_id,cht_id,content)
                LOCATION (Maybe Int,Maybe Int, Maybe Float,Maybe Float)|                 -- (upd_id,cht_id,latitude,longitude)
                OTHERMES (Maybe Int,Maybe Int)|                                          -- (upd_id,cht_id)
                NOINPUT  


(<|>) :: MESSAGE -> MESSAGE -> MESSAGE
(CONTACT (_,_,Nothing,_,_)) <|> r = r
(TEXT    (_,_,Nothing))     <|> r = r
(VIDEO   (_,_,Nothing))     <|> r = r
(AUDIO   (_,_,Nothing))     <|> r = r
(VOICE   (_,_,Nothing))     <|> r = r
(DOCUMENT(_,_,Nothing))     <|> r = r
(STICKER (_,_,Nothing))     <|> r = r
(CALLBACK(_,_,Nothing))     <|> r = r
(OTHERMES _)                <|> r = r
LOCATION (_,_,Nothing,_)    <|> r = r
(NOINPUT)                   <|> r = NOINPUT
l <|> r = l


data TelegramLocation =TelegramLocation
 { latitude :: Float,
   longitude :: Float 
 } deriving (Show,Generic)
instance FromJSON TelegramLocation where
instance ToJSON TelegramLocation where



newtype TelegramKeyBoard = TelegramKeyBoard
     { inline_keyboard :: [[String]]
     
     } deriving (Show,Generic)

instance FromJSON TelegramKeyBoard where
        parseJSON (Object v) = TelegramKeyBoard <$>
                              v .: "inline_keyboard"
        parseJSON _          = fail "not TelegramKeyBoard"
instance ToJSON TelegramKeyBoard where


newtype TelegramChat = TelegramChat
    { chat_message_id :: Int
    
    } deriving (Show,Generic)

instance FromJSON TelegramChat where
        parseJSON (Object v) = TelegramChat <$>
                                 v .: "id" 
        parseJSON _          = fail "not TelegramFrom"
instance ToJSON TelegramChat where


data TelegramMessage= TelegramMessage
    { message_id   :: Int
    , chat         :: Maybe TelegramChat
    , text         :: Maybe String
    , video        :: Maybe TelegramVideo
    , audio        :: Maybe TelegramAudio
    , sticker      :: Maybe TelegramSticker
    , document     :: Maybe TelegramDocument
    , voice        :: Maybe TelegramVoice
    , contact      :: Maybe TelegramContact
    , location     :: Maybe TelegramLocation
    } deriving (Show,Generic)

instance FromJSON TelegramMessage where
        parseJSON (Object v) = TelegramMessage <$>
                                 v .: "message_id" <*>
                                 v .:? "chat" <*>
                                 v .:? "text" <*>
                                 v .:? "video" <*>
                                 v .:? "audio" <*>
                                 v .:? "sticker" <*>
                                 v .:? "document" <*>
                                 v .:? "voice" <*>
                                 v .:? "contact" <*>
                                 v .:? "location"                               
        parseJSON _          = fail "not TelegramMessage"
instance ToJSON TelegramMessage where 

newtype TelegramVideo = TelegramVideo
   { file_id_video :: String
   } deriving (Show,Generic)

instance FromJSON TelegramVideo where
        parseJSON (Object v) = TelegramVideo <$>
                                 v .: "file_id"
        parseJSON _          = fail "not TelegramVideo"
instance ToJSON TelegramVideo where 


newtype TelegramAudio = TelegramAudio
  { file_id_audio :: String
  } deriving (Show,Generic)
instance FromJSON TelegramAudio where
        parseJSON (Object v) = TelegramAudio <$>
                                 v .: "file_id"
        parseJSON _          = fail "not TelegramAudio"
instance ToJSON TelegramAudio where 

newtype TelegramDocument = TelegramDocument 
  { file_id_doc :: String
  } deriving (Show,Generic)
instance FromJSON TelegramDocument where
        parseJSON (Object v) = TelegramDocument <$>
                                 v .: "file_id"
        parseJSON _          = fail "not TelegramDocument"
instance ToJSON TelegramDocument where 

newtype TelegramSticker = TelegramSticker 
  { file_id_sticker :: String
  } deriving (Show,Generic)
instance FromJSON TelegramSticker where
        parseJSON (Object v) = TelegramSticker <$>
                                 v .: "file_id"
        parseJSON _          = fail "not TelegramSticker"
instance ToJSON TelegramSticker where 

newtype TelegramVoice = TelegramVoice 
  { file_id_voice :: String
  } deriving (Show,Generic)
instance FromJSON TelegramVoice where
        parseJSON (Object v) = TelegramVoice <$>
                                 v .: "file_id"
        parseJSON _          = fail "not TelegramVoice"
instance ToJSON TelegramVoice where 

data TelegramContact = TelegramContact 
  { phone_number :: String,
    first_name   :: String,
    last_name    :: Maybe String,
    user_id      :: Maybe Integer
  } deriving (Show,Generic)
instance FromJSON TelegramContact where
instance ToJSON TelegramContact where 


newtype TelegramUser = TelegramUser
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


instance FromJSON TelegramUpdate where
        parseJSON (Object v) = TelegramUpdate <$>
                                 v .: "ok" <*>
                                 v .:? "result" <*>
                                 v .:? "decription" 
        parseJSON _          = fail "not TelegramUpdate"
instance ToJSON TelegramUpdate where