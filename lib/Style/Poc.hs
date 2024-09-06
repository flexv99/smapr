{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}

module Style.Poc where

import qualified Data.Aeson as A
import qualified Data.Aeson.Text as A
import qualified Data.ByteString.Lazy.Internal as B
import Data.Scientific (isFloating, toRealFloat)
import qualified Data.Text.Lazy as T
import qualified Data.Vector as V
import qualified Data.Sequence as S
import GHC.Generics
import Style.Parser
import Style.IsoExpressions
import Style.ExpressionsWrapper
import Style.ExpressionsEval
import Style.ExpressionsContext
import ApiClient
import Proto.Vector_tile.Tile.Layer (Layer(..))
import Proto.Vector_tile.Tile (Tile(..))
import Text.Megaparsec
import Proto.Util
import Util
import Style.Layers.Line
import Renderer.Geometry


-- The goal of this proof of concept is to correctly parse the style of this water way
-- and apply this style to my test vector tile unsing Render.Geomety.renderLayer.

data POCLayer = forall (b :: Bool). POCLayer
  { id :: T.Text
  , source :: T.Text
  , sourceLayer :: T.Text
  , lfilter :: ArgType ('SBool b)
  , paint :: LineS
  }
deriving instance Show POCLayer

data Width = Width
  { base :: Maybe SType
  , stops :: Maybe SType
  } deriving (Show, Eq, Generic)

-- Helper for use in combination with .:? to provide default values for optional JSON object fields.

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
      <*> obj A..: "paint"
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
waterLayerStyle = "{\"id\":\"waterway\",\"type\":\"line\",\"source\":\"openmaptiles\",\"source-layer\":\"waterway\",\"filter\":[\"all\",[\"==\",[\"geometry-type\"],\"LineString\"],[\"match\",[\"get\",\"brunnel\"],[\"bridge\",\"tunnel\"],false,true],[\"!=\",[\"get\",\"intermittent\"],1]],\"layout\":{\"visibility\":\"visible\"},\"paint\":{\"line-color\":\"hsl(205,56%,73%)\",\"line-opacity\":1,\"line-width\":[\"interpolate\",[\"exponential\",1.4],[\"zoom\"],8,1,20,8]}}"

transportationLayerStyle :: B.ByteString
transportationLayerStyle = "{\"id\":\"road_trunk_primary\",\"type\":\"line\",\"source\":\"openmaptiles\",\"source-layer\":\"transportation\",\"filter\":[\"all\",[\"==\",[\"geometry-type\"],\"LineString\"],[\"match\",[\"get\",\"class\"],[\"primary\",\"trunk\"],true,false]],\"layout\":{\"line-cap\":\"round\",\"line-join\":\"round\"},\"paint\":{\"line-color\":\"#fff\",\"line-width\":[\"interpolate\",[\"exponential\",1.4],[\"zoom\"],6,0.5,20,30]}}"

evalTester :: Maybe WrappedExpr -> IO (Maybe SType)
evalTester expr =
  testLayerAndFeature >>= (\ctx -> return (eval <$> expr <*> ctx))

evalLayer :: POCLayer -> ExpressionContext -> SType
evalLayer (POCLayer {lfilter = fltr}) = eval $ wrap fltr

constructCtx :: S.Seq Layer -> S.Seq ExpressionContext
constructCtx (l S.:<| xs) = create l S.>< constructCtx xs
  where
    create :: Layer -> S.Seq ExpressionContext
    create l' = fmap (\f -> ExpressionContext f l' 14 ) (features l')
constructCtx S.Empty      = S.empty

toBeDrawn :: Tile -> POCLayer -> S.Seq ExpressionContext
toBeDrawn t s = fmap (S.filter (unwrapSBool . evalLayer s)) constructCtx layers'
  where
   layers' = getLayers (T.unpack $ sourceLayer s) t

test :: IO ()
test = do
  t <- fakerTile
  let stile   = A.decode transportationLayerStyle :: Maybe POCLayer
  let tbD     = toBeDrawn <$> t <*> stile
  let diagram = (renderLayer . paint <$> stile) <*> tbD
  maybe (putStrLn "Nothing") writeSvg diagram

{-
>>> t <- fakerTile
>>> stile = A.decode "{\"id\":\"waterway\",\"type\":\"line\",\"source\":\"openmaptiles\",\"source-layer\":\"waterway\",\"filter\":[\"all\",[\"==\",[\"geometry-type\"],\"LineString\"],[\"match\",[\"get\",\"brunnel\"],[\"bridge\",\"tunnel\"],false,true],[\"!=\",[\"get\",\"intermittent\"],1]],\"layout\":{\"visibility\":\"visible\"},\"paint\":{\"line-color\":\"hsl(205,56%,73%)\",\"line-opacity\":1,\"line-width\":[\"interpolate\",[\"exponential\",1.4],[\"zoom\"],8,1,20,8]}}" :: Maybe POCLayer
>>> toBeDrawn <$> t <*> stileview lineCap
-}

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

{-
{
      "id": "waterway",
      "type": "line",
      "source": "openmaptiles",
      "source-layer": "waterway",
      "filter": [
        "all",
        ["==", ["geometry-type"], "LineString"],
        ["match", ["get", "brunnel"], ["bridge", "tunnel"], false, true],
        ["!=", ["get", "intermittent"], 1]
      ],
      "layout": {"visibility": "visible"},
      "paint": {
        "line-color": "hsl(205,56%,73%)",
        "line-opacity": 1,
        "line-width": [
          "interpolate",
          ["exponential", 1.4],
          ["zoom"],
          8,
          1,
          20,
          8
        ]
      }
    }
-}
