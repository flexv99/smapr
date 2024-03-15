{-# LANGUAGE OverloadedStrings #-}

module ApiClient where

import GHC.Float
import Network.Wreq
import Data.ByteString.Lazy.Internal
import Text.ProtocolBuffers (messageGet)
import Proto.Vector_tile.Tile (Tile(..))
import Control.Lens ((^.))

data Coord = Coord
  {
    lat :: Double
  , lon :: Double
  , zoom :: Double
  } deriving Show

baseUrl :: String
baseUrl = "http://0.0.0.0:3000/routes"

-- helpers0
lon2tileX :: (RealFrac a, Integral b, Floating a) => a -> a -> b
lon2tileX lon z = floor((lon + 180.0) / 360.0 * (2.0 ** z))

lat2tileY :: (RealFrac a, Integral b, Floating a) => a -> a -> b
lat2tileY lat z = floor((1.0 - log(tan(lat * pi / 180.0) + 1.0 / cos(lat * pi / 180.0)) / pi) / 2.0 * (2.0 ** z))

tilerequestUrl :: Coord -> String
tilerequestUrl c = baseUrl ++ "/" ++ show (double2Int (zoom c)) ++ "/" ++ show x ++ "/" ++ show y
  where
    x = lon2tileX (lon c) (zoom c)
    y = lat2tileY (lat c) (zoom c)

testCoord :: Coord
testCoord = Coord 46.615521 11.893506 14

transformRawTile :: ByteString -> Maybe Tile
transformRawTile raw = case messageGet raw of
    Left   _        -> Nothing
    Right (tile, _) -> Just tile

-- client
getTileUnserialized :: Coord -> IO (Response ByteString)
getTileUnserialized c = get (tilerequestUrl c)

getTile :: Coord -> IO (Maybe Tile)
getTile c = getTileUnserialized c >>=
  (\t -> return (transformRawTile (t ^. responseBody)))
