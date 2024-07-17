module Decoder.FeatureAttributes where

import GHC.Float (int2Double)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Foldable
import Data.Maybe
import GHC.Word
import qualified Data.Map as MP
import qualified Data.Sequence as S
import Text.ProtocolBuffers.Basic
import Proto.Vector_tile
import Proto.Vector_tile.Tile.Feature
import Proto.Vector_tile.Tile.GeomType
import Proto.Vector_tile.Tile.Layer
import Proto.Vector_tile.Tile.Value

data FeatureVal
  = ValStr String
  | ValFload Float
  | ValDouble Double
  | ValInt Int
  | ValBool Bool
  deriving (Show, Eq)

-- | mapping feature tags to key pairs 
featureProperties :: Layer -> [MP.Map String String]
featureProperties l = map (\x -> MP.fromList (key `zip` value)) (toList $ features l)
  where
    key = map (\(Utf8 s) -> unpack s) $ toList $ keys l
    value = map valueToFeatureVal $ toList $ values l
    valueToFeatureVal v = head $ catMaybes
      [ (\(Utf8 s) -> unpack s) <$> string_value v
      , show <$> float_value v
      , show <$> double_value v
      , show <$> int_value v
      , show <$> uint_value v
      , show <$> sint_value v
      , show <$> bool_value v]
