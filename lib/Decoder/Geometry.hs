module Decoder.Geometry (
  featureToGeo,
  decodeSeq,
  PolygonG (..),
  LineG (..),
  MapGeometry (..),
)
where

import Control.Monad.Reader
import Data.Foldable
import qualified Data.Sequence as S
import Decoder.Lines
import Decoder.Polygons
import GHC.Word
import Proto.Vector_tile.Tile.Feature
import Proto.Vector_tile.Tile.GeomType
import Style.ExpressionsContext

class MapGeometry a where
  decode :: [Int] -> [a]

instance MapGeometry PolygonG where
  decode = decPolygon

instance MapGeometry LineG where
  decode = decLine

featureToGeo :: (MapGeometry a, Show a) => Feature -> [a]
featureToGeo (Feature _ _ (Just POLYGON) g) = decode $ map fromIntegral $ toList g
featureToGeo (Feature _ _ (Just LINESTRING) g) = decode $ map fromIntegral $ toList g
featureToGeo f = decode (map fromIntegral $ toList $ geometry f)

decodeSeq :: (MapGeometry a) => S.Seq Word32 -> [a]
decodeSeq g = decode $ map fromIntegral $ toList g
