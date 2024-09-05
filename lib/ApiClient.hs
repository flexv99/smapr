{-# LANGUAGE OverloadedStrings #-}

module ApiClient where

import GHC.Float
import GHC.Word
import qualified Data.Sequence as S
import Data.Foldable
import Network.Wreq
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Internal
import qualified Data.ByteString as B
import Text.ProtocolBuffers (messageGet)
import Text.ProtocolBuffers.Basic (uToString)
import Proto.Vector_tile.Tile (Tile(..))
import Proto.Vector_tile.Tile.Layer (Layer(..))
import Proto.Vector_tile.Tile.Feature (Feature(..))
import Control.Lens ((^.))
import Util

data Coord = Coord
  { lat :: Double
  , lon :: Double
  , rZoom :: Double
  } deriving Show

-- helpers
lon2tileX :: (RealFrac a, Integral b, Floating a) => a -> a -> b
lon2tileX lon z = floor((lon + 180.0) / 360.0 * (2.0 ** z))

lat2tileY :: (RealFrac a, Integral b, Floating a) => a -> a -> b
lat2tileY lat z = floor((1.0 - log(tan(lat * pi / 180.0) + 1.0 / cos(lat * pi / 180.0)) / pi) / 2.0 * (2.0 ** z))

tilerequestUrl :: LocalApi -> Coord -> String
tilerequestUrl l c = base ++ "/" ++ show (double2Int (rZoom c)) ++ "/" ++ show x ++ "/" ++ show y
  where
    base = localBaseUrl l ++ linesPath l
    x = lon2tileX (lon c) (rZoom c)
    y = lat2tileY (lat c) (rZoom c)

nextzenTileUrl :: NextzenApi -> Coord -> String
nextzenTileUrl n c = nBaseUrl n ++ show (double2Int (rZoom c)) ++ "/" ++ show x ++ "/" ++ show y ++ suffix
  where
    suffix = "." ++ format n ++ "?api_key=" ++ apiKey n
    x = lon2tileX (lon c) (rZoom c)
    y = lat2tileY (lat c) (rZoom c)

testCoord :: Coord
testCoord = Coord 46.619221 11.893892 14 -- 46.615521 11.893506 14

transformRawTile :: ByteString -> Maybe Tile
transformRawTile raw = case messageGet raw of
    Left   _        -> Nothing
    Right (tile, _) -> Just tile

-- client
getTileUnserialized :: Coord -> IO (Response ByteString)
getTileUnserialized c = do
  conf <- smaprConfig
  get (tilerequestUrl (localApi conf) c)

getNextzenTileUnserialized :: Coord -> IO (Response ByteString)
getNextzenTileUnserialized c = do
  conf <- smaprConfig
  get (nextzenTileUrl (nextzenApi conf) c)

getTile :: Coord -> IO (Maybe Tile)
getTile c = getTileUnserialized c >>=
  (\t -> return (transformRawTile (t ^. responseBody)))

getNextzenTile :: Coord -> IO (Maybe Tile)
getNextzenTile c = getNextzenTileUnserialized c >>=
  (\t -> return (transformRawTile (t ^. responseBody)))

tileFeatures :: Tile -> [[Word32]]
tileFeatures t = map (toList . geometry) $ head
                 $ map (toList . features) $ toList $ layers t

getLayers :: String -> Tile -> S.Seq Layer
getLayers lName t = S.filter (\x -> uToString (name x) == lName)
                    $ layers t

filterLayerByName :: String -> Tile -> [[Word32]]
filterLayerByName lName t = map (toList . geometry)
                            $ head $ map (toList . features) $ toList
                                  $ getLayers lName t

fakerTile :: IO (Maybe Tile)
fakerTile = do
  conf <- smaprConfig
  let fp = testTilePath conf :: FilePath
  rawTile <- B.readFile fp
  return $ transformRawTile $ BL.fromStrict rawTile
