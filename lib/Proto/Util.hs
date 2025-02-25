{-# LANGUAGE OverloadedStrings #-}

module Proto.Util where

import ApiClient
import Control.Lens hiding (zoom)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Foldable
import qualified Data.Map as MP
import Data.Maybe
import Data.Scientific
import qualified Data.Sequence as S
import qualified Data.Text.Lazy as T
import GHC.Float
import GHC.Word
import Proto.Vector_tile.Tile
import Proto.Vector_tile.Tile.Feature
import Proto.Vector_tile.Tile.Layer
import Proto.Vector_tile.Tile.Value
import Style.ExpressionsContext
import Style.Lang.Types
import Style.Parser
import Text.ProtocolBuffers.Basic (uToString)
import qualified Text.ProtocolBuffers.Header as P'
import Prelude hiding (id)

geometryTypeToString :: Feature -> Maybe T.Text
geometryTypeToString f = T.pack . show <$> type' f

featureIdToString :: Feature -> Maybe T.Text
featureIdToString f = T.pack . show <$> id f

tuplify :: [a] -> [(a, a)]
tuplify [] = []
tuplify [_] = error "cannot tuplify single emelent"
tuplify (x : x' : xs) = (x, x') : tuplify xs

-- | mapping feature tags to key pairs
featureProperties :: ExpressionContext -> MP.Map T.Text SType
featureProperties ctx =
  MP.fromList
    $ map
      ( \(x, y) ->
          let (i, j) = (fromIntegral x, fromIntegral y)
           in (T.pack (key !! i), value !! j)
      )
    $ tuplify
    $ toList
    $ tags (ctx ^. feature)
  where
    key = map (\(P'.Utf8 s) -> unpack s) $ toList $ keys (ctx ^. layer)
    value = extractMappers $ toList $ values (ctx ^. layer)

-- | mapping feature tags to key pairs
featureProperties'' :: ExpressionContext -> MP.Map T.Text SData
featureProperties'' ctx =
  MP.fromList
    $ map
      ( \(x, y) ->
          let (i, j) = (fromIntegral x, fromIntegral y)
           in (T.pack (key !! i), value !! j)
      )
    $ tuplify
    $ toList
    $ tags (ctx ^. feature)
  where
    key = map (\(P'.Utf8 s) -> unpack s) $ toList $ keys (ctx ^. layer)
    value = extractMappers' $ toList $ values (ctx ^. layer)

extractMappers :: [Value] -> [SType]
extractMappers = concatMap extractMapper
  where
    extractMapper :: Value -> [SType]
    extractMapper v =
      concat
        [ maybeToList $ SString . (\(P'.Utf8 s) -> T.pack $ unpack s) <$> string_value v
        , maybeToList $ SNum . SDouble . float2Double <$> float_value v
        , maybeToList $ SNum . SDouble <$> double_value v
        , maybeToList $ SNum . SInt . fromIntegral <$> int_value v
        , maybeToList $ SNum . SInt . fromIntegral <$> uint_value v
        , maybeToList $ SNum . SInt . fromIntegral <$> sint_value v
        , maybeToList $ SBool <$> bool_value v
        ]

extractMappers' :: [Value] -> [SData]
extractMappers' = concatMap (filter filterValue . extractMapper)
  where
    extractMapper :: Value -> [SData]
    extractMapper v =
      [ DString $ (\(P'.Utf8 s) -> T.pack $ unpack s) <$> string_value v
      , DNum $ fromFloatDigits <$> float_value v
      , DNum $ fromFloatDigits <$> double_value v
      , DNum $ fromIntegral <$> int_value v
      , DNum $ fromIntegral <$> uint_value v
      , DNum $ fromIntegral <$> sint_value v
      , DBool $ bool_value v
      ]
    filterValue :: SData -> Bool
    filterValue (DString Nothing) = False
    filterValue (DNum Nothing) = False
    filterValue (DBool Nothing) = False
    filterValue _ = True

getLayers :: T.Text -> Tile -> S.Seq Layer
getLayers lName t =
  S.filter (\x -> uToString (name x) == T.unpack lName) $
    layers t

filterLayerByName :: T.Text -> Tile -> [[Word32]]
filterLayerByName lName t =
  map (toList . geometry) $
    head $
      map (toList . features) $
        toList $
          getLayers lName t

waterLayer :: IO (Maybe Layer)
waterLayer = fakerTile <&> fmap (\l -> getLayers "waterway" l `S.index` 0)

testLayerAndFeature :: IO (Maybe ExpressionContext)
testLayerAndFeature = do
  l <- waterLayer
  let f = fmap (`S.index` 0) (features <$> l)
  return $ ExpressionContext <$> f <*> l <*> Just 14
