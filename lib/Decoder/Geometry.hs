module Decoder.Geometry
  ( decPolygon,
    decLine,
    PolygonG (..),
    LineG (..),
    MapGeometry (..),
  )
where

import Data.Foldable
import Decoder.Lines
import Decoder.Polygons
import Proto.Vector_tile.Tile.Feature
import Proto.Vector_tile.Tile.GeomType

class MapGeometry a where
  decode :: [Int] -> [a]

instance MapGeometry PolygonG where
  decode = decPolygon

instance MapGeometry LineG where
  decode = decLine

featureToGeo :: (MapGeometry a, Show a) => Feature -> [a]
featureToGeo (Feature _ _ (Just POLYGON) g) = decode $ map fromIntegral $ toList g
featureToGeo (Feature _ _ (Just LINESTRING) g) = decode $ map fromIntegral $ toList g
featureToGeo f = decodeGeometry (map fromIntegral $ toList $ geometry f)

decodeGeometry :: (MapGeometry a) => [Int] -> [a]
decodeGeometry = decode
