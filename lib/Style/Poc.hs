{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Style.Poc where

import ApiClient
import qualified Data.Aeson as A
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Lazy as B
import Data.Colour.SRGB
import Data.Foldable
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import qualified Data.Sequence as S
import qualified Data.Text.Lazy as T
import Decoder.Geometry
import Decoder.Lines
import qualified Diagrams.Backend.SVG as D
import qualified Diagrams.Prelude as D
import GHC.Generics
import GHC.Word
import Graphics.Svg.Core
import Lens.Micro
import Proto.Util
import Proto.Vector
import Renderer.Geometry
import Style.Lang.Util
import Style.Layers.Background
import Style.Layers.Wrapper
import Util

-- The goal of this proof of concept is to correctly parse the style of this water way
-- and apply this style to my test vector tile unsing Render.Geomety.renderLayer.

data SWrap = SWrap
  { version :: Int
  , name :: T.Text
  , tlayers :: [SLayer]
  }
  deriving (Show, Generic)

instance A.FromJSON SWrap where
  parseJSON = A.withObject "Base" $ \o ->
    SWrap
      <$> o A..: "version"
      <*> o A..: "name"
      <*> o A..: "layers"

renderStyles
  :: forall {b}
   . (D.Renderable (D.Path D.V2 Double) b)
  => B.ByteString
  -> Tile
  -> Maybe (D.QDiagram b D.V2 Double D.Any)
renderStyles sts' t =
  let stile = A.decode sts' :: Maybe SLayer
      pt = (_paint =<< stile)
   in renderTile t <$> stile

split' :: [SLayer] -> ([SLayer], [SLayer])
split' layers = (l', f')
  where
    reverseList :: [a] -> [a]
    reverseList = foldl (flip (:)) []
    l' = reverseList $ filter (\x -> x ^. pType == "line") layers
    f' = reverseList $ filter (\x -> x ^. pType == "fill") layers

-- TODO add correct background
buildFinalDiagram'
  :: forall {b}
   . (D.Renderable (D.Path D.V2 Double) b)
  => [SLayer]
  -> Tile
  -> D.QDiagram b D.V2 Double D.Any
buildFinalDiagram' l t =
  D.bg
    (sRGB24 232 229 216)
    ( renderLayers'
        (fst splitted)
        `D.atop` renderLayers' (snd splitted)
    )
  where
    renderLayers' ls = mconcat (map (renderTile t) ls)
    bgP = head (filter (\x -> x ^. pType == "background") l) ^. paint
    bg (BackgroundPaint b) = b ^. backgroundColor
    bg _ = error "should not happen"
    splitted = split' l

pLayer :: IO (Either String SWrap)
pLayer = B.readFile "/home/flex99/tmp/osm.json" <&> A.eitherDecode

renderStyleSpec :: IO B.ByteString
renderStyleSpec = do
  t <- fakerTile
  stile <- B.readFile "/Users/felixvalentini/tmp/street1.json"
  let layy = tlayers <$> (A.eitherDecode stile :: Either String SWrap)
  let dg = buildFinalDiagram' <$> layy <*> t :: Either String (D.Diagram D.B)
  either
    (return . B.fromStrict . pack)
    ( return
        . renderBS
        . D.renderDia
          D.SVG
          (D.SVGOptions (D.mkWidth 512) Nothing "" [] True)
    )
    dg

-- renderWithCoords :: Coord -> IO ()
-- renderWithCoords coord = do
--   t <- getMTTile coord
--   stile <- B.readFile "/Users/felixvalentini/tmp/street1.json"
--   let layy = tlayers <$> (A.eitherDecode stile :: Either String SWrap)
--   let dg = buildFinalDiagram' <$> layy <*> t
--   either putStrLn writeSvg dg

-- debugRenderer :: Coord -> IO ()
-- debugRenderer c = do
--   t <- getMTTile c
--   stile <- B.readFile "/Users/felixvalentini/tmp/street1.json"
--   let layy = tlayers <$> (A.eitherDecode stile :: Either String SWrap)
--   let dg = buildFinalDiagram' <$> layy <*> (filterLayers "transportation" <$> t)
--   either putStrLn writeSvg dg

drawTour
  :: forall {b}
   . (D.Renderable (D.Path D.V2 Double) b)
  => [D.P2 Double]
  -> D.QDiagram b D.V2 Double D.Any
drawTour tour = tourPoints <> D.strokeP tourPath
  where
    tourPath = D.fromVertices tour
    tourPoints = D.atPoints (concat . D.pathVertices $ tourPath) (repeat dot)
    dot = D.circle 0.05 D.# D.fc D.black

-- featureToDiagramC :: Tile'Feature -> D.Diagram D.B
-- featureToDiagramC (Tile'Feature _ _ (Just LINESTRING) g) =
--   foldl1 D.atop $
--     map
--       (drawTour . lineToPoints)
--       (decodeC' g :: [LineG])
-- featureToDiagramC _ = D.strutX 0

lineToPoints :: LineG -> [D.P2 Double]
lineToPoints (LineG lMoveTo lLineTo) = _parameters lMoveTo ++ _parameters lLineTo

decodeC' :: (MapGeometry a) => S.Seq Word32 -> [a]
decodeC' g = decode $ map fromIntegral $ toList g

-- renderContourLayer :: T.Text -> Tile -> D.Diagram D.B
-- renderContourLayer l t =
--   D.reflectY
--     . foldl1 D.atop
--     . map featureToDiagram
--     . head
--     . map toList
--     . (map features <$> toList)
--     $ getLayers l t

-- testContour :: IO ()
-- testContour = do
--   t <- fakerTile
--   stile <- B.readFile "/home/flex99/dev/smapr/lib/Style/poc_style.json"
--   tc <- B.readFile "/home/flex99/tmp/contours_badia.pbf"
--   let tile = transformRawTile tc
--   let d = renderContourLayer "contour" <$> tile
--   let layy = tlayers <$> (A.decode stile :: Maybe SWrap)
--   let dg = buildFinalDiagram' <$> layy <*> t
--   maybe (putStrLn "Noting") writeSvg (d <> dg)

--- :break Renderer.Geometry.renderTile.eachLayer
