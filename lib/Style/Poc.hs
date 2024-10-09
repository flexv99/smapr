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

renderStyles :: B.ByteString -> Tile -> Maybe (D.Diagram D.B)
renderStyles sts' t =
  let stile = A.decode sts' :: Maybe SLayer
      pt = join $ _paint <$> stile
   in renderTile t <$> stile

renderStyles' :: SLayer -> Tile -> D.Diagram D.B
renderStyles' sts' t = renderTile t sts'

buildFinalDiagram' :: [SLayer] -> Tile -> D.Diagram D.B
buildFinalDiagram' l t =
  D.bg
    (sRGB24 232 229 216)
    (renderLayers' l)
  where
    renderLayers' ls = mconcat (map (`renderStyles'` t) ls)
    bg = head $ filter (\x -> x ^. pType == "background") l

pLayer :: IO (Either String SWrap)
pLayer = B.readFile "/home/flex99/tmp/osm.json" >>= return . A.eitherDecode

renderStyleSpec :: IO ()
renderStyleSpec = do
  t <- fakerTile
  stile <- B.readFile "/home/flex99/dev/smapr/lib/Style/poc_style.json"
  let layy = tlayers <$> (A.decode stile :: Maybe SWrap)
  let dg = buildFinalDiagram' <$> layy <*> t
  maybe (putStrLn "Noting") writeSvg dg

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
lineToPoints (LineG lMoveTo lLineTo) = _parameters lMoveTo ++ _parameters lLineTo

decodeC' :: (MapGeometry a) => S.Seq Word32 -> [a]
decodeC' g = decode $ map fromIntegral $ toList g

renderContourLayer :: T.Text -> Tile -> D.Diagram D.B
renderContourLayer l t = D.reflectY . foldl1 D.atop . map featureToDiagramC . head . map toList . (map features <$> toList) $ getLayers l t

testContour :: IO ()
testContour = do
  t <- fakerTile
  stile <- B.readFile "/home/flex99/dev/smapr/lib/Style/poc_style.json"
  tc <- B.readFile "/home/flex99/tmp/contours_badia.pbf"
  let tile = transformRawTile tc
  let d = renderContourLayer "contour" <$> tile
  let layy = tlayers <$> (A.decode stile :: Maybe SWrap)
  let dg = buildFinalDiagram' <$> layy <*> t
  maybe (putStrLn "Noting") writeSvg (d <> dg)
