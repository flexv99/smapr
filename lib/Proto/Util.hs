{-# LANGUAGE  OverloadedStrings #-}

module Proto.Util where

import Prelude hiding (id)
import Data.ByteString.Lazy.Char8 (unpack)
import Decoder.Helper (tuplify)
import Text.ProtocolBuffers.Basic (uToString)
import Control.Lens hiding (zoom)
import qualified Data.Text.Lazy as T
import qualified Text.ProtocolBuffers.Header as P'
import qualified Data.Map as MP
import qualified Data.Sequence as S
import Data.Foldable
import GHC.Float
import GHC.Word
import Data.Maybe
import Proto.Vector_tile.Tile
import Proto.Vector_tile.Tile.Value
import Proto.Vector_tile.Tile.Layer
import Proto.Vector_tile.Tile.Feature
import ApiClient
import Style.ExpressionsContext
import Style.Parser

geometryTypeToString :: Feature -> Maybe T.Text
geometryTypeToString f = T.pack . show <$> type' f

featureIdToString :: Feature -> Maybe T.Text
featureIdToString f = T.pack . show <$> id f

-- | mapping feature tags to key pairs
featureProperties :: ExpressionContext -> MP.Map T.Text SType
featureProperties ctx = MP.fromList $ map (\(x, y) -> let (i, j) = (fromIntegral x, fromIntegral y)
                                           in ( T.pack (key !! i), value !! j)) $ tuplify $ toList $ tags (ctx ^. feature)
  where
    key = map (\(P'.Utf8 s) -> unpack s) $ toList $ keys (ctx ^. layer)
    value = extractMappers $ toList $ values (ctx ^. layer)

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

getLayers :: String -> Tile -> S.Seq Layer
getLayers lName t = S.filter (\x -> uToString (name x) == lName)
                    $ layers t

filterLayerByName :: String -> Tile -> [[Word32]]
filterLayerByName lName t = map (toList . geometry)
                            $ head $ map (toList . features) $ toList
                                  $ getLayers lName t

waterLayer :: IO (Maybe Layer)
waterLayer = fakerTile <&> fmap (\l -> getLayers "waterway" l `S.index` 0)

testLayerAndFeature :: IO (Maybe ExpressionContext)
testLayerAndFeature = do
  l <- waterLayer
  let f = fmap (`S.index` 0) (features <$> l)
  return $ ExpressionContext <$> f <*> l <*> Just 14
