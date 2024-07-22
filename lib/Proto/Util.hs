module Proto.Util where

import Prelude hiding (id)
import Data.ByteString.Lazy.Char8 (unpack)
import qualified Data.Text.Lazy as T
import qualified Text.ProtocolBuffers.Header as P'
import qualified Data.Map as MP
import qualified Data.Sequence as S
import Data.Foldable
import GHC.Word
import Data.Maybe
import Proto.Vector_tile.Tile.Value
import Proto.Vector_tile.Tile.GeomType
import Proto.Vector_tile.Tile.Layer
import Proto.Vector_tile.Tile.Feature

data FeatureVal
  = ValStr String
  | ValFload Float
  | ValDouble Double
  | ValInt Int
  | ValBool Bool
  deriving (Show, Eq)

geometryTypeToString :: Feature -> Maybe T.Text
geometryTypeToString f = T.pack . show <$> type' f

featureIdToString :: Feature -> Maybe T.Text
featureIdToString f = T.pack . show <$> id f

-- | mapping feature tags to key pairs 
featureProperties :: Layer -> [MP.Map String String]
featureProperties l = map (\x -> MP.fromList (key `zip` value)) (toList $ features l)
  where
    key = map (\(P'.Utf8 s) -> unpack s) $ toList $ keys l
    value = map valueToFeatureVal $ toList $ values l
    valueToFeatureVal v = head $ catMaybes
      [ (\(P'.Utf8 s) -> unpack s) <$> string_value v
      , show <$> float_value v
      , show <$> double_value v
      , show <$> int_value v
      , show <$> uint_value v
      , show <$> sint_value v
      , show <$> bool_value v]
