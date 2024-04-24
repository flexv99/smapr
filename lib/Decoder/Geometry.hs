module Decoder.Geometry 
  ( decPolygon
  , decLine
  , PolygonG(..)
  , LineG(..)
  , MapGeometry(..) ) where

import Data.Bits
import Control.Monad
import GHC.Float (int2Double)
import GHC.Word
import Data.Foldable
import qualified Data.Sequence as S
import Proto.Vector_tile
import Proto.Vector_tile.Tile.GeomType
import Proto.Vector_tile.Tile.Layer
import Proto.Vector_tile.Tile.Feature
import qualified Data.Aeson as A
import Decoder.Polygons
import Decoder.Lines

class MapGeometry a where
  decode :: [Int] -> [a]

instance MapGeometry PolygonG where
  decode = decPolygon

instance MapGeometry LineG where
  decode = decLine

featureToGeo :: (MapGeometry a, Show a) => Feature -> [a]
featureToGeo (Feature _ _ (Just POLYGON) g)    = decode $ map fromIntegral $ toList g
featureToGeo (Feature _ _ (Just LINESTRING) g) = decode $ map fromIntegral $ toList g
featureToGeo f                                 = decodeGeometry (map fromIntegral $ toList $ geometry f)

decodeGeometry :: (MapGeometry a) => [Int] -> [a]
decodeGeometry = decode
