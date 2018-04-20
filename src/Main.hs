{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.HTTP.Simple
import Data.Aeson
import Conduit (MonadThrow)
import System.Environment (getArgs)
import qualified Data.ByteString as BS
import Network.Mail.SMTP
import Data.Text.Lazy
type APIKey = String

defaultConfigName :: String 
defaultConfigName = "settings.json"

data WeatherSettings = WeatherSettings {
  key :: APIKey,
  state :: String,
  city :: String
}

instance FromJSON WeatherSettings where 
  parseJSON (Object v) = WeatherSettings 
    <$> v .: "api_key"
    <*> v .: "default_state"
    <*> v .: "default_city"

data WeatherResponse = WeatherResponse {
  precip1Hr :: String,
  precipToday :: String
}

instance FromJSON WeatherResponse where 
  parseJSON (Object v) = do 
    current <- v .: "current_observation"
    WeatherResponse
      <$> current .: "precip_1hr_in"
      <*> current .: "precip_today_in"

main :: IO ()
main = do
  eitherWs <- eitherDecodeStrict <$> BS.readFile defaultConfigName
  case eitherWs of 
    Left err -> putStrLn err 
    Right ws -> do
      req <- buildRequest ws
      res <- getResponseBody <$> httpJSON req :: IO WeatherResponse
      putStrLn $ "Expected precipitation within 1hr: " ++ precip1Hr res
      putStrLn $ "Expected precipitation today: " ++ precipToday res
      sendEmail res

-- http://api.wunderground.com/api/YOUR_API_KEY/conditions/q/THE_DESIRED_STATE/THE_DESIRED_CITY.json
buildRequest :: (MonadThrow m) => WeatherSettings -> m Request
buildRequest settings = do 
  let url = "http://api.wunderground.com/api/" ++ key settings ++ "/conditions/q/" ++ state settings ++ "/" ++ city settings ++ ".json"
  parseRequest url

-- auth: https://api.twilio.com/2010-04-01/Accounts
-- 
sendSMS :: WeatherSettings -> WeatherResponse -> IO ()
sendSMS ws wr = do 
  
                  
