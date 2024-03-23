{-# LANGUAGE OverloadedStrings #-}

module ApiClient where

import GHC.Float
import GHC.Word
import Control.Monad
import Control.Applicative
import Data.Sequence
import Data.Foldable
import Network.Wreq
import Data.ByteString.Lazy.Internal
import Text.ProtocolBuffers (messageGet)
import Proto.Vector_tile.Tile (Tile(..))
import Proto.Vector_tile.Tile.Layer (Layer(..))
import Proto.Vector_tile.Tile.Feature (Feature(..))
import Control.Lens ((^.))
import Util

data Coord = Coord
  { lat :: Double
  , lon :: Double
  , zoom :: Double
  } deriving Show

host :: IO (String)
host = smaprConfig >>= return . baseUrl

-- helpers0
lon2tileX :: (RealFrac a, Integral b, Floating a) => a -> a -> b
lon2tileX lon z = floor((lon + 180.0) / 360.0 * (2.0 ** z))

lat2tileY :: (RealFrac a, Integral b, Floating a) => a -> a -> b
lat2tileY lat z = floor((1.0 - log(tan(lat * pi / 180.0) + 1.0 / cos(lat * pi / 180.0)) / pi) / 2.0 * (2.0 ** z))

tilerequestUrl :: String -> Coord -> String
tilerequestUrl base c = base ++ "/" ++ show (double2Int (zoom c)) ++ "/" ++ show x ++ "/" ++ show y
  where
    x = lon2tileX (lon c) (zoom c)
    y = lat2tileY (lat c) (zoom c)

testCoord :: Coord
testCoord = Coord 46.615521 11.893506 14

transfromRawTile :: ByteString -> Maybe Tile
transfromRawTile raw = case messageGet raw of
    Left   _        -> Nothing
    Right (tile, _) -> Just tile

-- client
getTileUnserialized :: Coord -> IO (Response ByteString)
getTileUnserialized c = do
  conf <- smaprConfig
  let base = (baseUrl conf) ++ (linesPath conf)
  get (tilerequestUrl base c)

getTile :: Coord -> IO (Maybe Tile)
getTile c = getTileUnserialized c >>=
  (\t -> return (transfromRawTile (t ^. responseBody)))

tileFeatures :: Tile -> [[Word32]]
tileFeatures t = map (toList . geometry) $ head $ map (\x -> toList $ features x) $ toList $ layers t

-- getTile testCoord >>= \t -> return $ liftM tileFeatures t
