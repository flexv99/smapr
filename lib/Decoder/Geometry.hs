module Decoder.Geometry (
  decodeVec,
  PolygonG (..),
  LineG (..),
  MapGeometry (..),
)
where

import qualified Data.Vector.Unboxed as V
import Decoder.Lines
import Decoder.Polygons
import GHC.Word

class MapGeometry a where
  decode :: [Int] -> [a]

instance MapGeometry PolygonG where
  decode = decPolygon

instance MapGeometry LineG where
  decode = decLine

decodeVec :: (MapGeometry a) => V.Vector Word32 -> [a]
decodeVec = decode . V.toList . V.map fromIntegral
