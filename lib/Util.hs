{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Util
  ( smaprConfig
  , writeSvg
  , Sconf(..)
  , LocalApi(..)
  , NextzenApi(..)
  )
where

import Graphics.Svg
import GHC.Generics (Generic)
import Data.Time.Clock (getCurrentTime, UTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C

data LocalApi = LocalApi
  { localBaseUrl :: String
  , linesPath :: String
  } deriving (Show, Generic)

data NextzenApi = NextzenApi
  { nBaseUrl :: String
  , apiKey :: String
  , format :: String
  } deriving (Show, Generic)

data Sconf = Sconf
  { localApi :: LocalApi
  , nextzenApi :: NextzenApi
  , testTilePath :: String
  , testDestinationPath :: String
  } deriving (Show, Generic)

makeLocalApiConf :: C.Config -> IO (Maybe LocalApi)
makeLocalApiConf conf = do
  localBaseUrl   <- C.lookup conf "api.base_url" :: IO (Maybe String)
  linesPath      <- C.lookup conf "api.lines_path" :: IO (Maybe String)
  return $ LocalApi <$> localBaseUrl <*> linesPath

makeNextzenApiConf :: C.Config -> IO (Maybe NextzenApi)
makeNextzenApiConf conf = do
  nBaseUrl   <- C.lookup conf "nextzen_api.base_url" :: IO (Maybe String)
  apiKey     <- C.lookup conf "nextzen_api.api_key" :: IO (Maybe String)
  format     <- C.lookup conf "nextzen_api.format" :: IO (Maybe String)
  return $ NextzenApi <$> nBaseUrl <*> apiKey <*> format

makeSconf :: C.Config -> IO (Maybe Sconf)
makeSconf conf = do
  localApi             <- makeLocalApiConf conf :: IO (Maybe LocalApi)
  nextzenApi           <- makeNextzenApiConf conf :: IO (Maybe NextzenApi)
  testDestinationPath  <- C.lookup conf "test_dest_path" :: IO (Maybe String)
  testTilePath         <- C.lookup conf "test_tile_path" :: IO (Maybe String)
  return $ Sconf <$> localApi
                 <*> nextzenApi
                 <*> testTilePath
                 <*> testDestinationPath

smaprConfig :: IO Sconf
smaprConfig = do
  loadedConf <- C.load [C.Required "smapr.cfg"]
  sConf      <- makeSconf loadedConf
  case sConf of
    Nothing   -> error "invalid config file"
    Just conf -> return conf

testPath :: String -> IO FilePath
testPath context = smaprConfig >>= \conf -> return (testDestinationPath conf ++ context  ++ ".svg" :: FilePath)

dateTimeStr :: IO String
dateTimeStr = getCurrentTime >>=
  \currentTime -> return $ formatTime defaultTimeLocale "%Y-%m-%d_%H:%M:%S" currentTime

writeSvg :: Element -> IO ()
writeSvg svg = do
  dateString <- dateTimeStr
  destPath   <- testPath dateString
  putStrLn destPath
  renderToFile destPath svg
