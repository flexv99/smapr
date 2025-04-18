{-# LANGUAGE OverloadedStrings #-}

module Proto.Util where

import ApiClient
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Foldable
import qualified Data.Map as MP
import Data.Maybe
import Data.ProtoLens
import Data.Scientific
import qualified Data.Sequence as S
import qualified Data.Text.Lazy as T
import qualified Data.Vector as V
import GHC.Word
import Lens.Micro
import Proto.Vector
import Proto.Vector_Fields
import Style.ExpressionsContext
import Style.Lang.Types
import Prelude hiding (id)

geometryTypeToString :: Tile'Feature -> T.Text
geometryTypeToString f = T.pack $ showEnum $ f ^. type'

featureIdToString :: Tile'Feature -> T.Text
featureIdToString f = T.pack $ show $ f ^. id

tuplify :: [a] -> [(a, a)]
tuplify [] = []
tuplify [_] = error "cannot tuplify single emelent"
tuplify (x : x' : xs) = (x, x') : tuplify xs

-- | mapping feature tags to key pairs
featureProperties'' :: ExpressionContext -> MP.Map T.Text SData
featureProperties'' ctx =
  MP.fromList
    $ map
      ( \(x, y) ->
          let (i, j) = (fromIntegral x, fromIntegral y)
           in (fromMaybe "" (key !? i), fromMaybe (DNum Nothing) (value !? j))
      )
    $ tuplify
    $ toList
      (ctx ^. (feature . tags))
  where
    key = V.map T.fromStrict $ ctx ^. (layer . vec'keys)
    value = extractMappers' (ctx ^. (layer . values))
    xs !? n
      | n < 0 = Nothing
      | otherwise =
          foldr
            ( \x r k -> case k of
                0 -> Just x
                _ -> r (k - 1)
            )
            (const Nothing)
            xs
            n

extractMappers' :: [Tile'Value] -> [SData]
extractMappers' = concatMap (filter filterValue . extractMapper)
  where
    extractMapper :: Tile'Value -> [SData]
    extractMapper v =
      [ DString $ T.fromStrict <$> (v ^. maybe'stringValue)
      , DNum $ fromFloatDigits <$> (v ^. maybe'floatValue)
      , DNum $ fromFloatDigits <$> (v ^. maybe'doubleValue)
      , DNum $ fromIntegral <$> (v ^. maybe'intValue)
      , DNum $ fromIntegral <$> (v ^. maybe'uintValue)
      , DNum $ fromIntegral <$> (v ^. maybe'sintValue)
      , DBool $ v ^. maybe'boolValue
      ]
    filterValue :: SData -> Bool
    filterValue (DString Nothing) = False
    filterValue (DNum Nothing) = False
    filterValue (DBool Nothing) = False
    filterValue _ = True

getLayers :: T.Text -> Tile -> [Tile'Layer]
getLayers lName t =
  filter
    (\x -> T.fromStrict (x ^. name) == lName)
    $ toList (t ^. layers)

filterLayers :: T.Text -> Tile -> Tile
filterLayers lName t = defMessage & layers .~ getLayers lName t
