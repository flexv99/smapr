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
import Style.IsoExpressions
import Style.FeatureExpressions
import Style.ExpressionsWrapper
import ApiClient
import Proto.Vector_tile.Tile.Layer (Layer(..))
import Text.Megaparsec
import Proto.Util
import Style.Layers.Line

-- The goal of this proof of concept is to correctly parse the style of this water way
-- and apply this style to my test vector tile unsing Render.Geomety.renderLayer.

data POCLayer = forall (b :: Bool). POCLayer
  { id :: T.Text
  , source :: T.Text
  , sourceLayer :: T.Text
  , lfilter :: ArgType ('SBool b)
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
      then pure $ SNum $ SDouble (toRealFloat n)
      else pure $ SNum $ SInt (round n)
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
      <*> obj A..: "source"
      <*> obj A..: "source-layer"
      <*> (obj A..: "filter" >>= fexpr)
      <*> obj A..:? "paint"
    where
      fexpr  = A.withArray "FilterExpression" $ \v ->
        case parse allP "" (A.encodeToLazyText v) of
          Left err  -> fail $ errorBundlePretty err
          Right res -> pure res

tpaint :: B.ByteString
tpaint = "{\"line-color\":\"hsl(205, 56%, 73%)\",\"line-opacity\":1,\"line-width\":{\"base\":1.4,\"stops\":[[8,1],[20,8]]}}"

tfilter :: B.ByteString
tfilter = "[\"all\",[\"==\", [\"geometry-type\"], \"Polygon\"],[\"!=\", [\"get\", \"intermittent\"], 1],[\"!=\", [\"get\", \"brunnel\"], \"tunnel\"]]"

waterLayerStyle :: B.ByteString
waterLayerStyle = "{\"version\":8,\"name\":\"Basic\",\"metadata\":{\"mapbox:autocomposite\":false,\"mapbox:type\":\"template\",\"maputnik:renderer\":\"mbgljs\",\"openmaptiles:version\":\"3.x\",\"openmaptiles:mapbox:owner\":\"openmaptiles\",\"openmaptiles:mapbox:source:url\":\"mapbox://openmaptiles.4qljc88t\"},\"sources\":{\"openmaptiles\":{\"type\":\"vector\",\"url\":\"https://api.maptiler.com/tiles/v3-openmaptiles/tiles.json?key={key}\"}},\"sprite\":\"https://openmaptiles.github.io/maptiler-basic-gl-style/sprite\",\"glyphs\":\"https://api.maptiler.com/fonts/{fontstack}/{range}.pbf?key={key}\",\"layers\":[{\"id\":\"water\",\"type\":\"fill\",\"source\":\"openmaptiles\",\"source-layer\":\"water\",\"filter\":[\"all\",[\"==\",[\"geometry-type\"],\"Polygon\"],[\"!=\",[\"get\",\"intermittent\"],1],[\"!=\",[\"get\",\"brunnel\"],\"tunnel\"]],\"layout\":{\"visibility\":\"visible\"},\"paint\":{\"fill-color\":\"hsl(205,56%,73%)\"}}],\"id\":\"basic\"}"

-- >>> A.eitherDecode "{\"id\":\"water\",\"type\":\"fill\",\"source\":\"openmaptiles\",\"source-layer\":\"water\",\"filter\":[\"all\",[\"==\",[\"geometry-type\"],\"Polygon\"],[\"!=\",[\"get\",\"intermittent\"],1],[\"!=\",[\"get\",\"brunnel\"],\"tunnel\"]],\"layout\":{\"visibility\":\"visible\"},\"paint\":{\"fill-color\":\"hsl(205,56%,73%)\"}}" :: Either String POCLayer
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
