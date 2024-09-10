{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Util
  ( smaprConfig
  , testPath
  , dateTimeStr
  , writeSvg
  , Sconf(..)
  , LocalApi(..)
  , MTApi(..)
  ) where

import GHC.Generics (Generic)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import qualified Diagrams.Prelude as D
import qualified Diagrams.Backend.SVG as D

data LocalApi = LocalApi
  { localBaseUrl :: String
  , path        :: String
  , format'      :: String
  } deriving (Show, Generic)

data MTApi = MTApi
  { nBaseUrl :: String
  , apiKey   :: String
  , format   :: String
  } deriving (Show, Generic)

data Sconf = Sconf
  { localApi     :: LocalApi
  , mtApi   :: MTApi
  , testTilePath :: String
  , testDestinationPath :: String
  } deriving (Show, Generic)

makeLocalApiConf :: C.Config -> IO (Maybe LocalApi)
makeLocalApiConf conf = do
  localBaseUrl'   <- C.lookup conf "api.base_url" :: IO (Maybe String)
  path'           <- C.lookup conf "api.path" :: IO (Maybe String)
  suffix          <- C.lookup conf "api.format" :: IO (Maybe String)
  return $ LocalApi <$> localBaseUrl' <*> path' <*> suffix 

makeNextzenApiConf :: C.Config -> IO (Maybe MTApi)
makeNextzenApiConf conf = do
  nBaseUrl'   <- C.lookup conf "maptiler_api.base_url" :: IO (Maybe String)
  apiKey'     <- C.lookup conf "maptiler_api.api_key" :: IO (Maybe String)
  format'     <- C.lookup conf "maptiler_api.format" :: IO (Maybe String)
  return $ MTApi <$> nBaseUrl' <*> apiKey' <*> format'

makeSconf :: C.Config -> IO (Maybe Sconf)
makeSconf conf = do
  localApi'             <- makeLocalApiConf conf :: IO (Maybe LocalApi)
  nextzenApi'           <- makeNextzenApiConf conf :: IO (Maybe MTApi)
  testDestinationPath'  <- C.lookup conf "test_dest_path" :: IO (Maybe String)
  testTilePath'         <- C.lookup conf "test_tile_path" :: IO (Maybe String)
  return $ Sconf <$> localApi'
                 <*> nextzenApi'
                 <*> testTilePath'
                 <*> testDestinationPath'

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

writeSvg :: D.Diagram D.B -> IO ()
writeSvg d = do
  let sz = D.mkSizeSpec2D (Just 512) (Just 512)
  dateStr <- dateTimeStr
  path <- testPath dateStr
  putStrLn path
  D.renderSVG path sz d
