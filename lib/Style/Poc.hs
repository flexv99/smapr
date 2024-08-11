{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}

module Style.Poc where

import qualified Data.Aeson as A
import qualified Data.Aeson.Text as A
import qualified Data.ByteString.Lazy.Internal as B
import Data.Scientific (isFloating, toRealFloat)
import qualified Data.Text.Lazy as T
import Data.Colour (transparent)
import Data.Functor ((<&>))
import qualified Data.Vector as V
import qualified Data.Sequence as S
import GHC.Generics
import Style.Parser
import Style.Expressions
import Style.FilterExpressions
import ApiClient
import Proto.Vector_tile.Tile.Layer (Layer(..))
import Text.Megaparsec
import Proto.Util
import Style.Layers.Line

-- The goal of this proof of concept is to correctly parse the style of this water way
-- and apply this style to my test vector tile unsing Render.Geomety.renderLayer.

data POCLayer = forall (b :: Bool). POCLayer
  { id :: T.Text
  , layerType :: T.Text
  , source :: T.Text
  , sourceLayer :: T.Text
  , lfilter :: FilterExpr ('SBool b)
  , paint :: Maybe LineS
  }
deriving instance Show POCLayer

data Width = Width
  { base :: Maybe SType
  , stops :: Maybe SType
  } deriving (Show, Eq, Generic)

-- Helper for use in combination with .:? to provide default values for optional JSON object fields.

instance A.FromJSON SType where
  parseJSON (A.Number n) =
    if isFloating n
      then pure $ SDouble (toRealFloat n)
      else pure $ SInt (round n)
  parseJSON (A.Bool b)   = pure $ SBool b
  parseJSON (A.Array a)  = SArray <$> traverse A.parseJSON (V.toList a)
  parseJSON a            = A.withText
      "SType"
      ( \v ->
          case parse pAtom "" (T.fromStrict v) of
            Left err  -> fail $ errorBundlePretty err
            Right res -> return res
      ) a

instance A.FromJSON Width where
  parseJSON = A.withObject "Width" $ \obj ->
    Width
      <$> obj A..:? "base"
      <*> obj A..:? "stops"

instance A.FromJSON POCLayer where
  parseJSON = A.withObject "POCLayer" $ \obj ->
    POCLayer
      <$> obj A..: "id"
      <*> obj A..: "type"
      <*> obj A..: "source"
      <*> obj A..: "source-layer"
      <*> (obj A..: "filter" >>= fexpr)
      <*> obj A..:? "paint"
    where
      fexpr  = A.withArray "FilterExpression" $ \v ->
        case parse filterParsers "" (A.encodeToLazyText v) of
          Left err  -> fail $ errorBundlePretty err
          Right res -> pure res

tpaint :: B.ByteString
tpaint = "{\"line-color\":\"hsl(205, 56%, 73%)\",\"line-opacity\":1,\"line-width\":{\"base\":1.4,\"stops\":[[8,1],[20,8]]}}"

tfilter :: B.ByteString
tfilter = "[\"all\",[\"==\", [\"geometry-type\"], \"Polygon\"],[\"!=\", [\"get\", \"intermittent\"], 1],[\"!=\", [\"get\", \"brunnel\"], \"tunnel\"]]"

twaterway :: B.ByteString
twaterway = "{\"id\":\"waterway\",\"type\":\"line\",\"source\":\"openmaptiles\",\"source-layer\":\"waterway\",\"filter\":[\"all\",[\"==\",\"$type\",\"LineString\"],[\"!in\",\"brunnel\",\"tunnel\",\"bridge\"]],\"paint\":{\"line-color\":\"hsl(205, 56%, 73%)\",\"line-opacity\":1,\"line-width\":{\"base\":1.4,\"stops\":[[8,1],[20,8]]}}}"

{-
{
      "id": "water",
      "type": "fill",
      "source": "openmaptiles",
      "source-layer": "water",
      "filter": [
        "all",
        ["==", ["geometry-type"], "Polygon"],
        ["!=", ["get", "intermittent"], 1],
        ["!=", ["get", "brunnel"], "tunnel"]
      ],
      "layout": {"visibility": "visible"},
      "paint": {"fill-color": "hsl(205,56%,73%)"}
    }
-}
