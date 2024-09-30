module Decoder.Geometry
  ( featureToGeo,
    PolygonG (..),
    LineG (..),
    MapGeometry (..),
  )
where

import Control.Monad.Reader
import Data.Foldable
import Decoder.Lines
import Decoder.Polygons
import Proto.Vector_tile.Tile.Feature
import Proto.Vector_tile.Tile.GeomType
import Style.ExpressionsContext

data MapGeometry = LineGeo [LineG] | PolyGeo PolygonG deriving (Show)

featureToGeo :: Feature -> MapGeometry
featureToGeo (Feature _ _ (Just POLYGON) g) = LineGeo $ decLine $ map fromIntegral $ toList g
featureToGeo (Feature _ _ (Just LINESTRING) g) = PolyGeo $ decPolygon $ map fromIntegral $ toList g
featureToGeo f = LineGeo $ decLine (map fromIntegral $ toList $ geometry f)
