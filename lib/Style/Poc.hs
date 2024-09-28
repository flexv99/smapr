{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Style.Poc where

import ApiClient
import Control.Lens
import Control.Monad
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as B
import Data.Colour.SRGB
import Data.Foldable
import Data.Maybe
import qualified Data.Sequence as S
import qualified Data.Text.Lazy as T
import Decoder.Geometry
import Decoder.Lines
import qualified Diagrams.Backend.SVG as D
import qualified Diagrams.Prelude as D
import GHC.Generics
import GHC.Word
import Proto.Util
import Proto.Vector_tile.Tile
import Proto.Vector_tile.Tile.Feature
import Proto.Vector_tile.Tile.GeomType
import Proto.Vector_tile.Tile.Layer
import Renderer.Geometry
import Style.ExpressionsWrapper
import Style.IsoExpressions
import Style.Layers.Wrapper
import Style.Parser
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

data SWrap = SWrap
  { version :: Int,
    name :: T.Text,
    tlayers :: [SLayer]
  }
  deriving (Show, Generic)

instance A.FromJSON SWrap where
  parseJSON = A.withObject "Base" $ \o ->
    SWrap
      <$> o A..: "version"
      <*> o A..: "name"
      <*> o A..: "layers"

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
l2 = "{\"id\":\"landcover_grass\",\"type\":\"fill\",\"source\":\"openmaptiles\",\"source-layer\":\"landcover\",\"filter\":[\"==\",[\"get\",\"class\"],\"grass\"],\"paint\":{\"fill-color\":\"hsl(47, 26%, 88%)\",\"fill-opacity\":0.45}}"

l3 :: B.ByteString
l3 = "{\"id\":\"landcover_wood\",\"type\":\"fill\",\"source\":\"openmaptiles\",\"source-layer\":\"landcover\",\"filter\":[\"==\",[\"get\",\"class\"],\"wood\"],\"paint\":{\"fill-color\":\"hsl(82,46%,72%)\",\"fill-opacity\":[\"interpolate\",[\"linear\"],[\"zoom\"],8,0.6,22,1]}}"

l4 :: B.ByteString
l4 = "{\"id\":\"landcover_sand\",\"type\":\"fill\",\"metadata\":{},\"source\":\"openmaptiles\",\"source-layer\":\"landcover\",\"filter\":[\"match\",[\"get\",\"class\"],[\"sand\"],true,false],\"paint\":{\"fill-antialias\":false,\"fill-color\":\"rgba(232,214,38,1)\",\"fill-opacity\":0.3}}"

testLayers :: [B.ByteString]
testLayers = [waterLayerStyle, waterFill, transportationLayerStyle, buildingsLayerStyle, l1, l3, l2, l4]

testEval :: (SParseable a) => T.Text -> IsoExpr a -> Tile -> [a]
testEval layer expr t = map (eval expr) ctxs
  where
    layers = getLayers layer t
    ctxs = toList $ constructCtx layers

renderStyles :: B.ByteString -> Tile -> Maybe (D.Diagram D.B)
renderStyles sts' t =
  let stile = A.decode sts' :: Maybe SLayer
      tbD = toBeDrawn t <$> stile
      pt = join $ _paint <$> stile
   in (renderLayer <$> pt) <*> tbD

renderStyles' :: SLayer -> Tile -> Maybe (D.Diagram D.B)
renderStyles' sts' t =
  let tbD = toBeDrawn t sts'
      pt = sts' ^. paint
   in fmap (`renderLayer` tbD) pt

split' :: [SLayer] -> ([SLayer], [SLayer])
split' layers = (l', f')
  where
    reverseList :: [a] -> [a]
    reverseList = foldl (flip (:)) []
    l' = reverseList $ filter (\x -> x ^. pType == "line") layers
    f' = reverseList $ filter (\x -> x ^. pType == "fill") layers

buildFinalDiagram :: Tile -> D.Diagram D.B
buildFinalDiagram t = D.bg (sRGB24 232 229 216) (foldl D.atop (D.strutX 0) (mapMaybe (`renderStyles` t) testLayers))

buildFinalDiagram' :: [SLayer] -> Tile -> D.Diagram D.B
buildFinalDiagram' l t =
  D.bg
    (sRGB24 232 229 216)
    ( renderLayers'
        (fst splitted)
        `D.atop` renderLayers' (snd splitted)
    )
  where
    renderLayers' ls = mconcat (mapMaybe (`renderStyles'` t) ls)
    bg = head $ filter (\x -> x ^. pType == "background") l
    splitted = split' l

test :: IO ()
test = do
  t <- fakerTile
  maybe (putStrLn "Noting") (writeSvg . buildFinalDiagram) t

testWithUrl :: String -> IO ()
testWithUrl url = do
  t <- getFromUrl url
  maybe (putStrLn "Noting") (writeSvg . buildFinalDiagram) t

pLayer :: IO (Either String SWrap)
pLayer = B.readFile "/home/flex99/tmp/osm.json" >>= return . A.eitherDecode

renderStyleSpec :: IO ()
renderStyleSpec = do
  t <- fakerTile
  stile <- B.readFile "/home/flex99/dev/smapr/lib/Style/poc_style.json"
  tc <- B.readFile "/home/flex99/tmp/contours_badia.pbf"
  let tile = transformRawTile tc
  let d = renderContourLayer "contour" <$> tile
  let layy = tlayers <$> (A.decode stile :: Maybe SWrap)
  let dg = buildFinalDiagram' <$> layy <*> t
  maybe (putStrLn "Noting") writeSvg (d <> dg)

renderStyleSpecWithUrl :: String -> IO ()
renderStyleSpecWithUrl url = do
  t <- getFromUrl url
  stile <- B.readFile "/home/flex99/dev/smapr/lib/Style/poc_style.json"
  let layy = tlayers <$> (A.decode stile :: Maybe SWrap)
  let dg = buildFinalDiagram' <$> layy <*> t
  maybe (putStrLn "Noting") writeSvg dg

drawTour :: [D.P2 Double] -> D.Diagram D.B
drawTour tour = tourPoints <> D.strokeP tourPath
  where
    tourPath = D.fromVertices tour
    tourPoints = D.atPoints (concat . D.pathVertices $ tourPath) (repeat dot)
    dot = D.circle 0.05 D.# D.fc D.black

featureToDiagramC :: Feature -> D.Diagram D.B
featureToDiagramC (Feature _ _ (Just LINESTRING) g) = foldl1 D.atop $ map (drawTour . lineToPoints) (decodeC' g :: [LineG])
featureToDiagramC _ = D.strutX 0

lineToPoints :: LineG -> [D.P2 Double]
lineToPoints (LineG lMoveTo lLineTo) = toDPoint $ _parameters lMoveTo ++ _parameters lLineTo
  where
    toDPoint = map geometryPointToDPoint
    geometryPointToDPoint :: Point -> D.P2 Double
    geometryPointToDPoint (x, y) = x D.^& y

decodeC' :: (MapGeometry a) => S.Seq Word32 -> [a]
decodeC' g = decode $ map fromIntegral $ toList g

renderContourLayer :: T.Text -> Tile -> D.Diagram D.B
renderContourLayer l t = D.reflectY . foldl1 D.atop . map featureToDiagramC . head . map toList . (map features <$> toList) $ getLayers l t

testContour :: IO ()
testContour = do
  t <- B.readFile "/home/flex99/tmp/contours_badia.pbf"
  let tile = transformRawTile t
  let d = renderContourLayer "contour" <$> tile
  maybe (putStrLn "Nothing") writeSvg d
