{-# LANGUAGE  OverloadedStrings #-}

module Proto.Util where

import Prelude hiding (id)
import Data.ByteString.Lazy.Char8 (unpack)
import Decoder.Helper (tuplify)
import Data.Functor ((<&>))
import qualified Data.Text.Lazy as T
import qualified Data.Text.Encoding as TE
import qualified Text.ProtocolBuffers.Header as P'
import qualified Data.Map as MP
import qualified Data.Sequence as S
import Data.Foldable
import GHC.Word
import GHC.Float
import Data.Maybe
import Proto.Vector_tile.Tile.Value
import Proto.Vector_tile.Tile.GeomType
import Proto.Vector_tile.Tile.Layer
import Proto.Vector_tile.Tile.Feature
import ApiClient
import Style.Parser

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
featureProperties :: Layer -> Feature -> MP.Map T.Text SType
featureProperties l f = MP.fromList $ map (\(x, y) -> let (i, j) = (fromIntegral x, fromIntegral y)
                                           in ( T.pack (key !! i), value !! j)) $ tuplify $ toList $ tags f
  where
    key = map (\(P'.Utf8 s) -> unpack s) $ toList $ keys l
    value = extractMappers $ toList $ values l


extractMappers :: [Value] -> [SType]
extractMappers = concatMap extractMapper
  where
    extractMapper :: Value -> [SType]
    extractMapper v = concat [ maybeToList $ SString . (\(P'.Utf8 s) -> T.pack $ unpack s) <$> string_value v
      , maybeToList $ SNum . SDouble . float2Double <$> float_value v
      , maybeToList $ SNum . SDouble <$> double_value v
      , maybeToList $ SNum . SInt . fromIntegral <$> int_value v
      , maybeToList $ SNum . SInt . fromIntegral <$> uint_value v
      , maybeToList $ SNum . SInt . fromIntegral <$> sint_value v
      , maybeToList $ SBool <$> bool_value v
      ]

waterLayer :: IO (Maybe Layer)
waterLayer = fakerTile <&> fmap (\l -> getLayers "waterway" l `S.index` 0)

testLayerAndFeature :: IO (Maybe Layer, Maybe Feature)
testLayerAndFeature = do
  testLayer <- waterLayer
  let f = fmap (`S.index` 0) (features <$> testLayer)
  return (testLayer, f)

