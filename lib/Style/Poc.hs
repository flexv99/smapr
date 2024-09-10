{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}

module Style.Poc where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Internal as B
import qualified Diagrams.Prelude as D
import qualified Diagrams.Backend.SVG as D
import Util
import Text.Megaparsec
import Data.Colour.SRGB
import GHC.Generics
import Data.Maybe
import Style.Parser
import Style.ExpressionsWrapper
import Style.ExpressionsEval
import Proto.Vector_tile.Tile
import ApiClient
import Proto.Util
import Renderer.Geometry
import Style.Layers.Wrapper


-- The goal of this proof of concept is to correctly parse the style of this water way
-- and apply this style to my test vector tile unsing Render.Geomety.renderLayer.

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



waterLayerStyle :: B.ByteString
waterLayerStyle = "{\"id\":\"waterway\",\"type\":\"line\",\"source\":\"openmaptiles\",\"source-layer\":\"waterway\",\"filter\":[\"all\",[\"==\",[\"geometry-type\"],\"LineString\"],[\"match\",[\"get\",\"brunnel\"],[\"bridge\",\"tunnel\"],false,true],[\"!=\",[\"get\",\"intermittent\"],1]],\"layout\":{\"visibility\":\"visible\"},\"paint\":{\"line-color\":\"hsl(205,56%,73%)\",\"line-opacity\":1,\"line-width\":[\"interpolate\",[\"exponential\",1.4],[\"zoom\"],8,1,20,8]}}"

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
testLayers = [waterLayerStyle, transportationLayerStyle, buildingsLayerStyle, l1, l2, l3]

evalTester :: Maybe WrappedExpr -> IO (Maybe SType)
evalTester expr =
  testLayerAndFeature >>= (\ctx -> return (eval <$> expr <*> ctx))

renderStyles :: B.ByteString -> Tile -> Maybe (D.Diagram D.B)
renderStyles sts' t =
  let stile   = A.decode sts' :: Maybe SLayer
      tbD     = toBeDrawn t <$> stile
  in (renderLayer . paint <$> stile) <*> tbD

test :: IO ()
test = do
  t <- getTile (Coord 46.4953588 11.3375709 14)
  let renderedLayers = catMaybes $ mapMaybe (\x -> renderStyles x <$> t) testLayers
  let diagram = D.bg (sRGB24 232 229 216) (foldl1 D.atop renderedLayers)
  writeSvg diagram
  -- maybe (putStrLn "Nothing") writeSvg diagram
