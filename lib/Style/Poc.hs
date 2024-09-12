{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Style.Poc where

import ApiClient
import Control.Monad
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Internal as B
import Data.Colour.SRGB
import Data.Foldable
import Data.Maybe
import qualified Data.Sequence as S
import qualified Diagrams.Backend.SVG as D
import qualified Diagrams.Prelude as D
import GHC.Generics
import Proto.Util
import Proto.Vector_tile.Tile
import Proto.Vector_tile.Tile.Layer
import Renderer.Geometry
import Style.ExpressionsContext
import Style.ExpressionsEval
import Style.ExpressionsWrapper
import Style.FeatureExpressions
import Style.IsoExpressions
import Style.Layers.Wrapper
import Style.Parser
import Text.Megaparsec
import Util

-- The goal of this proof of concept is to correctly parse the style of this water way
-- and apply this style to my test vector tile unsing Render.Geomety.renderLayer.

data Width = Width
  { base :: Maybe SType,
    stops :: Maybe SType
  }
  deriving (Show, Eq, Generic)

-- Helper for use in combination with .:? to provide default values for optional JSON object fields.

instance A.FromJSON Width where
  parseJSON = A.withObject "Width" $ \obj ->
    Width
      <$> obj A..:? "base"
      <*> obj A..:? "stops"

waterLayerStyle :: B.ByteString
waterLayerStyle = "{\"id\":\"waterway\",\"type\":\"line\",\"source\":\"openmaptiles\",\"source-layer\":\"waterway\",\"filter\":[\"all\",[\"==\",[\"geometry-type\"],\"LineString\"],[\"match\",[\"get\",\"brunnel\"],[\"bridge\",\"tunnel\"],false,true],[\"!=\",[\"get\",\"intermittent\"],1]],\"layout\":{\"visibility\":\"visible\"},\"paint\":{\"line-color\":\"hsl(205,56%,73%)\",\"line-opacity\":1,\"line-width\":[\"interpolate\",[\"exponential\",1.4],[\"zoom\"],8,1,20,8]}}"

waterFill :: B.ByteString
waterFill = "{\"id\":\"water\",\"type\":\"fill\",\"source\":\"openmaptiles\",\"source-layer\":\"water\",\"filter\":[\"all\",[\"==\",[\"geometry-type\"],\"Polygon\"],[\"!=\",[\"get\",\"intermittent\"],1],[\"!=\",[\"get\",\"brunnel\"],\"tunnel\"]],\"layout\":{\"visibility\":\"visible\"},\"paint\":{\"fill-color\":\"hsl(205,56%,73%)\"}}"

transportationLayerStyle :: B.ByteString
transportationLayerStyle = "{\"id\":\"road_trunk_primary\",\"type\":\"line\",\"source\":\"openmaptiles\",\"source-layer\":\"transportation\",\"filter\":[\"all\",[\"==\",[\"geometry-type\"],\"LineString\"],[\"match\",[\"get\",\"class\"],[\"primary\",\"trunk\"],true,false]],\"layout\":{\"line-cap\":\"round\",\"line-join\":\"round\"},\"paint\":{\"line-color\":\"#fff\",\"line-width\":[\"interpolate\",[\"exponential\",1.4],[\"zoom\"],6,0.5,20,30]}}"

buildingsLayerStyle :: B.ByteString
buildingsLayerStyle = "{\"id\":\"building\",\"type\":\"fill\",\"source\":\"openmaptiles\",\"source-layer\":\"building\",\"paint\":{\"fill-antialias\":true,\"fill-color\":\"rgba(222,211,190,1)\",\"fill-opacity\":[\"interpolate\",[\"linear\"],[\"zoom\"],13,0,15,1],\"fill-outline-color\":[\"interpolate\",[\"linear\"],[\"zoom\"],15,\"rgba(212,177,146,0)\",16,\"rgba(212,177,146,0.5)\"]}}"

l1 :: B.ByteString
l1 = "{\"id\":\"landuse-residential\",\"type\":\"fill\",\"source\":\"openmaptiles\",\"source-layer\":\"landuse\",\"filter\":[\"all\",[\"==\",[\"geometry-type\"],\"Polygon\"],[\"match\",[\"get\",\"class\"],[\"neighbourhood\",\"residential\",\"suburb\"],true,false]],\"layout\":{\"visibility\":\"visible\"},\"paint\":{\"fill-color\":\"hsl(47,13%,86%)\",\"fill-opacity\":0.7}}"

l2 :: B.ByteString
l2 = "{\"id\":\"landcover_grass\",\"type\":\"fill\",\"source\":\"openmaptiles\",\"source-layer\":\"landcover\",\"filter\":[\"==\",[\"get\",\"class\"],\"grass\"],\"paint\":{\"fill-color\":\"hsl(82,46%,72%)\",\"fill-opacity\":0.45}}"

l3 :: B.ByteString
l3 = "{\"id\":\"landcover_wood\",\"type\":\"fill\",\"source\":\"openmaptiles\",\"source-layer\":\"landcover\",\"filter\":[\"==\",[\"get\",\"class\"],\"wood\"],\"paint\":{\"fill-color\":\"hsl(82,46%,72%)\",\"fill-opacity\":[\"interpolate\",[\"linear\"],[\"zoom\"],8,0.6,22,1]}}"

testLayers :: [B.ByteString]
testLayers = [waterLayerStyle, waterFill, transportationLayerStyle, buildingsLayerStyle, l1, l2, l3]

testEval :: String -> WrappedExpr -> Tile -> [SType]
testEval layer expr t = map (eval expr) ctxs
  where
    layers = getLayers layer t
    ctxs = toList $ constructCtx layers

renderStyles :: B.ByteString -> Tile -> Maybe (D.Diagram D.B)
renderStyles sts' t =
  let stile = A.decode sts' :: Maybe SLayer
      tbD = toBeDrawn t <$> stile
   in (renderLayer . paint <$> stile) <*> tbD

buildFinalDiagram :: Tile -> D.Diagram D.B
buildFinalDiagram t = D.bg (sRGB24 232 229 216) (foldl D.atop (D.strutX 0) (mapMaybe (`renderStyles` t) testLayers))

test :: IO ()
test = do
  t <- getMTTile (Coord 46.785019 11.931154 14)
  maybe (putStrLn "Noting") (writeSvg . buildFinalDiagram) t

testWithUrl :: String -> IO ()
testWithUrl url = do
  t <- getFromUrl url
  maybe (putStrLn "Noting") (writeSvg . buildFinalDiagram) t
