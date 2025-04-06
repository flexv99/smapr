{-# LANGUAGE OverloadedStrings #-}

module ApiClient where

import Control.Lens ((^.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Internal
import Data.Foldable
import GHC.Float
import GHC.Word
import Network.HTTP.Client (Response, httpLbs, newManager, parseRequest, responseBody)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Proto.Vector_tile.Tile (Tile (..))
import Proto.Vector_tile.Tile.Feature (Feature (..))
import Proto.Vector_tile.Tile.Layer (Layer (..))
import Text.ProtocolBuffers (messageGet)
import Util

data Coord = Coord
  { lat :: Double
  , lon :: Double
  , rZoom :: Double
  }
  deriving (Show)

-- helpers
lon2tileX :: (RealFrac a, Integral b, Floating a) => a -> a -> b
lon2tileX lon' z = floor ((lon' + 180.0) / 360.0 * (2.0 ** z))

lat2tileY :: (RealFrac a, Integral b, Floating a) => a -> a -> b
lat2tileY lat' z = floor ((1.0 - log (tan (lat' * pi / 180.0) + 1.0 / cos (lat' * pi / 180.0)) / pi) / 2.0 * (2.0 ** z))

tilerequestUrl :: LocalApi -> Coord -> String
tilerequestUrl l c = base ++ "/" ++ show (double2Int (rZoom c)) ++ "/" ++ x ++ "/" ++ y ++ suffix
  where
    suffix = "." ++ format' l
    base = localBaseUrl l ++ path l
    x = show (lon2tileX (lon c) (rZoom c) :: Int)
    y = show (lat2tileY (lat c) (rZoom c) :: Int)

mTTileUrl :: MTApi -> Coord -> String
mTTileUrl n c = nBaseUrl n ++ show (double2Int (rZoom c)) ++ "/" ++ x ++ "/" ++ y ++ suffix
  where
    suffix = "." ++ format n ++ "?key=" ++ apiKey n
    x = show (lon2tileX (lon c) (rZoom c) :: Int)
    y = show (lat2tileY (lat c) (rZoom c) :: Int)

testCoord :: Coord
testCoord = Coord 46.619221 11.893892 14 -- 46.615521 11.893506 14

transformRawTile :: ByteString -> Maybe Tile
transformRawTile raw = case messageGet raw of
  Left _ -> Nothing
  Right (tile, _) -> Just tile

-- client
getTileUnserialized :: Coord -> IO (Response ByteString)
getTileUnserialized c = do
  conf <- smaprConfig
  manager <- newManager tlsManagerSettings
  request <- parseRequest $ tilerequestUrl (localApi conf) c
  httpLbs request manager

getMTTileUnserialized :: Coord -> IO (Response ByteString)
getMTTileUnserialized c = do
  conf <- smaprConfig
  manager <- newManager tlsManagerSettings
  request <- parseRequest (mTTileUrl (mtApi conf) c)
  httpLbs request manager

getTile :: Coord -> IO (Maybe Tile)
getTile c =
  getTileUnserialized c
    >>= (return . transformRawTile . responseBody)

getMTTile :: Coord -> IO (Maybe Tile)
getMTTile c =
  getMTTileUnserialized c
    >>= (return . transformRawTile . responseBody)

tileFeatures :: Tile -> [[Word32]]
tileFeatures t =
  map (toList . geometry) $
    head $
      map (toList . features) $
        toList $
          layers t

fakerTile :: IO (Maybe Tile)
fakerTile = do
  conf <- smaprConfig
  let fp = testTilePath conf :: FilePath
  rawTile <- BL.readFile fp
  return $ transformRawTile rawTile
