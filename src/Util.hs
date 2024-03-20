{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Util
  ( smaprConfig
  , writeSvg
  , Sconf(..)
  )
where

import Graphics.Svg
import GHC.Generics (Generic)
import Data.Time.Clock (getCurrentTime, UTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C

data Sconf = Sconf
  { baseUrl :: String
  , linesPath :: String
  , testDestinationPath :: String
  } deriving (Show, Generic)

makeSconf :: C.Config -> IO (Maybe Sconf)
makeSconf conf = do
  baseUrl             <- C.lookup conf "api.base_url" :: IO (Maybe String)
  linesPath           <- C.lookup conf "api.lines_path" :: IO (Maybe String)
  testDestinationPath <- C.lookup conf "test_dest_path" :: IO (Maybe String)
  return $ Sconf <$> baseUrl
                 <*> linesPath
                 <*> testDestinationPath

smaprConfig :: IO (Sconf)
smaprConfig = do
  loadedConf <- C.load [C.Required "smapr.cfg"]
  sConf      <- makeSconf loadedConf
  case sConf of
    Nothing   -> error "invalid config file"
    Just conf -> return conf

testPath :: String -> IO (FilePath)
testPath context = smaprConfig >>= \conf -> return ((testDestinationPath conf) ++ context  ++ ".svg" :: FilePath)

dateTimeStr :: IO String
dateTimeStr = getCurrentTime >>=
  \currentTime -> return $ formatTime defaultTimeLocale "%Y-%m-%d_%H:%M:%S" currentTime

writeSvg :: Element -> IO ()
writeSvg svg = do
  dateString <- dateTimeStr
  destPath   <- testPath dateString
  putStrLn destPath
  renderToFile destPath svg
