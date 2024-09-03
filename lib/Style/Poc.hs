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
import qualified Data.Colour as C
import Data.Foldable (toList)
import Data.Functor ((<&>))
import qualified Data.Vector as V
import qualified Data.Sequence as S
import qualified Diagrams.Prelude as D
import qualified Diagrams.Backend.SVG as D
import qualified Diagrams.Trail as D
import qualified Diagrams.TwoD.Size as D
import qualified Diagrams.Located as D
import Control.Lens
import GHC.Generics
import Style.Parser
import Style.IsoExpressions
import Style.FeatureExpressions
import Style.ExpressionsWrapper
import Style.ExpressionsEval
import ApiClient
import Proto.Vector_tile.Tile.Layer (Layer(..))
import Proto.Vector_tile.Tile.Feature (Feature(..))
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

evalLayer :: POCLayer -> Feature -> Layer -> SType
evalLayer (POCLayer {lfilter = fltr}) = eval $ wrap fltr

evalTester :: Maybe WrappedExpr -> IO (Maybe SType)
evalTester expr =
  testLayerAndFeature >>= (\(l, f) -> return (eval <$> expr <*> f <*> l))

toBeDrawn :: Tile -> POCLayer -> S.Seq Feature
toBeDrawn t s = iter layers
  where
   layers = getLayers (T.unpack $ sourceLayer s) t
   iter :: S.Seq Layer -> S.Seq Feature
   iter (l S.:<| xs) = S.filter (\f -> unwrapSBool $ evalLayer s f l) (features l) S.>< iter xs
   iter S.Empty      = S.empty

withStyle :: forall {b}. D.HasStyle b => LineS -> b -> b
withStyle style d = d
  D.# D.lineCap (style ^. lineCap)
  -- D.# D.lc (unwrapC (style ^. lineColor))

  

test :: IO ()
test = do
  t <- fakerTile
  let stile   = A.decode waterLayerStyle :: Maybe POCLayer
  let tbD     = toBeDrawn <$> t <*> stile
  let diagram = renderLayer' . toList <$> tbD
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
