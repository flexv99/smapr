module Decoder.Geometry (
  featureToGeo,
  decodeSeq,
  PolygonG (..),
  LineG (..),
  MapGeometry (..),
)
where

import Data.Foldable
import Data.ProtoLens.Prism
import qualified Data.Sequence as S
import Decoder.Lines
import Decoder.Polygons
import GHC.Word
import Lens.Micro
import Proto.Vector
import Proto.Vector_Fields

class MapGeometry a where
  decode :: [Int] -> [a]

instance MapGeometry PolygonG where
  decode = decPolygon

instance MapGeometry LineG where
  decode = decLine

featureToGeo :: (MapGeometry a, Show a) => Tile'Feature -> [a]
featureToGeo f = undefined

-- featureToGeo (Feature _ _ (Just POLYGON) g) = decode $ map fromIntegral $ toList g
-- featureToGeo (Tile'Feature _ _ (Just LINESTRING) g) = decode $ map fromIntegral $ toList g
-- featureToGeo f = decode (map fromIntegral $ toList $ geometry f)

decodeSeq :: (MapGeometry a) => S.Seq Word32 -> [a]
decodeSeq g = decode $ map fromIntegral $ toList g
