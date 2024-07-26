{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

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
import ApiClient
import Proto.Vector_tile.Tile.Layer (Layer(..))
import Text.Megaparsec
import Proto.Util

-- The goal of this proof of concept is to correctly parse the style of this water way
-- and apply this style to my test vector tile unsing Render.Geomety.renderLayer.

data POCLayer = POCLayer
  { id :: T.Text
  , layerType :: T.Text
  , source :: T.Text
  , sourceLayer :: T.Text
  , filter :: T.Text
  , paint :: Maybe POCPaint
  } deriving (Show, Eq, Generic)

data POCPaint = POCPaint
  { lineColor :: Maybe SType
  , lineOpacity :: Maybe SType
  , base :: Maybe SType
  , stops :: Maybe [SType]
  , lineWidth :: Maybe POCPaint
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



instance A.FromJSON POCPaint where
  parseJSON = A.withObject "POCPaint" $ \obj ->
    POCPaint
      <$> obj A..:? "line-color"
      <*> obj A..:? "line-opacity"
      <*> obj A..:? "base"
      <*> obj A..:? "stops"
      <*> obj A..:? "line-width"

instance A.FromJSON POCLayer where
  parseJSON = A.withObject "POCLayer" $ \obj ->
    POCLayer
      <$> obj A..: "id"
      <*> obj A..: "type"
      <*> obj A..: "source"
      <*> obj A..: "source-layer"
      <*> obj A..: "filter"
      <*> obj A..:? "paint"

tpaint :: B.ByteString
tpaint = "{\"line-color\":\"hsl(205, 56%, 73%)\",\"line-opacity\":1,\"line-width\":{\"base\":1.4,\"stops\":[[8,1],[20,8]]}}"

tfilter :: B.ByteString
tfilter = "[\"all\",[\"==\",\"$type\",\"LineString\"],[\"!in\",\"brunnel\",\"tunnel\",\"bridge\"]]"


twaterway :: B.ByteString
twaterway = "{\"id\":\"waterway\",\"type\":\"line\",\"source\":\"openmaptiles\",\"source-layer\":\"waterway\",\"filter\":[\"all\",[\"==\",\"$type\",\"LineString\"],[\"!in\",\"brunnel\",\"tunnel\",\"bridge\"]],\"paint\":{\"line-color\":\"hsl(205, 56%, 73%)\",\"line-opacity\":1,\"line-width\":{\"base\":1.4,\"stops\":[[8,1],[20,8]]}}}"

{-
  {
  "id": "waterway",
  "type": "line",
  "source": "openmaptiles",
  "source-layer": "waterway",
  "filter": [
    "all",
    ["==", "$type", "LineString"],
    ["!in", "brunnel", "tunnel", "bridge"]
  ],
  "paint": {
    "line-color": "hsl(205, 56%, 73%)",
    "line-opacity": 1,
    "line-width": {"base": 1.4, "stops": [[8, 1], [20, 8]]}
  }
},
-}
waterLayer :: IO (Maybe Layer)
waterLayer = fakerTile <&> fmap (\l -> getLayers "water" l `S.index` 0)

