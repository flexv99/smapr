{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module ApiClient where

import Control.Monad (mapM, sequence)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Functor ((<&>))
import Data.ProtoLens
import GHC.Float
import GHC.Word
import Lens.Micro
import Network.HTTP.Client hiding (path)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Proto.Vector
import Proto.Vector_Fields
import Util

data Coord = Coord
  { lat :: Double
  , lon :: Double
  , rZoom :: Double
  }
  deriving (Show)

data FiveTiles = FiveTiles
  { center :: Tile -- (x, y)
  , top :: Tile -- (x, y - 1)
  , bottom :: Tile -- (x, y + 1)
  , left :: Tile -- (x - 1, y)
  , right :: Tile -- (x + 1, y)
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
testCoord = Coord 46.615521 11.893506 14

transformRawTile :: BL.ByteString -> Either String Tile
transformRawTile raw = decodeMessage $ B.toStrict raw

-- client
getTileUnserialized :: Coord -> IO (Response BL.ByteString)
getTileUnserialized c = do
  conf <- smaprConfig
  manager <- newManager tlsManagerSettings
  request <- parseRequest (tilerequestUrl (localApi conf) c)
  httpLbs request manager

getMTTileUnserialized :: Coord -> IO (Response BL.ByteString)
getMTTileUnserialized c = do
  conf <- smaprConfig
  manager <- newManager tlsManagerSettings
  request <- parseRequest (mTTileUrl (mtApi conf) c)
  httpLbs request manager

getTile :: Coord -> IO (Either String Tile)
getTile c =
  getTileUnserialized c <&> (transformRawTile . responseBody)

getMTTile :: Coord -> IO (Either String Tile)
getMTTile c =
  getMTTileUnserialized c <&> (transformRawTile . responseBody)

getFromUrl :: String -> IO (Either String Tile)
getFromUrl url = do
  manager <- newManager tlsManagerSettings
  request <- parseRequest url
  httpLbs request manager <&> (transformRawTile . responseBody)

fakerTile :: IO (Either String Tile)
fakerTile = do
  conf <- smaprConfig
  let fp = testTilePath conf :: FilePath
  rawTile <- BL.readFile fp
  return $ transformRawTile rawTile

getFiveTiles :: Coord -> IO (Either String [Tile])
getFiveTiles c = do
  conf <- smaprConfig
  manager <- newManager tlsManagerSettings
  let api = mtApi conf
  let req = map (\p -> nBaseUrl api ++ p ++ "?key=" ++ apiKey api) (urls c)
  sequence <$> (mapM (\url -> getFromUrl url) req)
  where
    urls :: Coord -> [String]
    urls c =
      map
        (\(a, b) -> stringified (x + a) (y + b))
        [ ((-1), (-1))
        , (0, (-1))
        , (1, (-1))
        , ((-1), 0)
        , (0, 0)
        , (1, 0)
        , ((-1), 1)
        , (0, 1)
        , (1, 1)
        ]
      where
        x = lon2tileX (lon c) (rZoom c)
        y = lat2tileY (lat c) (rZoom c)
        stringified :: Int -> Int -> String
        stringified x y = show (double2Int (rZoom c)) ++ "/" ++ show x ++ "/" ++ show y ++ ".pbf"
