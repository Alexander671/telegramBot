{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Network.HTTP.Simple            ( httpBS, getResponseBody )               
import qualified Data.ByteString.Char8         as BS
import Lib


main :: IO ()
main = do
    json <- fetchJSON
    BS.putStrLn json

fetchJSON :: IO BS.ByteString
fetchJSON = do
    res <- httpBS "https://api.telegram.org/bot1283054130:AAFMfS1-WADlOtvj77jOm9COzDc8D-JLJIE/getMe"
    return (getResponseBody res)
