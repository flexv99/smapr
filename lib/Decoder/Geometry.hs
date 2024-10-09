module Decoder.Geometry
  ( decodeSeq,
    PolygonG (..),
    LineG (..),
    MapGeometry (..),
  )
where

import Data.Foldable
import qualified Data.Sequence as S
import Decoder.Lines
import Decoder.Polygons
import GHC.Word
import Proto.Vector_tile.Tile.Feature
import Proto.Vector_tile.Tile.GeomType

class MapGeometry a where
  decode :: [Int] -> [a]

instance MapGeometry PolygonG where
  decode = decPolygon

instance MapGeometry LineG where
  decode = decLine

decodeSeq :: (MapGeometry a) => S.Seq Word32 -> [a]
decodeSeq g = decode $ map fromIntegral $ toList g
