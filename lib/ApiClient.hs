{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module ApiClient where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Functor ((<&>))
import Data.ProtoLens
import GHC.Float
import GHC.Word
import Lens.Micro
import Proto.Vector
import Proto.Vector_Fields
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
testCoord = Coord 46.615521 11.893506 14

transformRawTile :: BL.ByteString -> Either String Tile
transformRawTile raw = decodeMessage $ B.toStrict raw

fakerTile :: IO (Either String Tile)
fakerTile = do
  conf <- smaprConfig
  let fp = testTilePath conf :: FilePath
  rawTile <- BL.readFile fp
  return $ transformRawTile rawTile
