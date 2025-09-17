{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module View where

import ApiClient
import Control.Monad (join)
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as B
import Data.Colour.SRGB
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text.Lazy as T
import Decoder.Geometry
import Decoder.Lines
import qualified Diagrams.Backend.SVG as D
import qualified Diagrams.Prelude as D
import GHC.Generics
import Graphics.Svg.Core (renderBS)
import Lens.Micro
import Proto.Util
import Proto.Vector
import Renderer.Geometry
import Style.Lang.Util
import Style.Layers.Wrapper
import System.Directory
import Util

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

split' :: [SLayer] -> ([SLayer], [SLayer], [SLayer])
split' layers = (l', f', p')
  where
    reverseList :: [a] -> [a]
    reverseList = foldl (flip (:)) []
    l' = reverseList $ filter (\x -> x ^. pType == "line") layers
    f' = reverseList $ filter (\x -> x ^. pType == "fill") layers
    p' = reverseList $ filter (\x -> x ^. pType == "symbol") layers

buildFinalDiagram' :: [SLayer] -> Tile -> D.Diagram D.B
buildFinalDiagram' l t =
  D.bg
    (background)
    ( renderLayers'
        (splitted ^. _3)
        `D.atop` renderLayers' (splitted ^. _1)
        `D.atop` renderLayers' (splitted ^. _2)
    )
  where
    background =
      maybe (sRGB24 232 229 216) pureColor ((`renderBg` t) =<< firstLayerByType l)
    renderLayers' ls = mconcat (map (renderTile t) ls)
    firstLayerByType = listToMaybe . filter (\x -> x ^. pType == "background")
    splitted = split' l

renderWithCoords :: Coord -> IO ()
renderWithCoords coord = do
  fp <- getCurrentDirectory
  t <- getMTTile coord
  stile <- B.readFile  "/Users/flex99/map/styles/dataviz.json" -- $ fp <> "/poc_style.json"
  let layy = tlayers <$> (A.eitherDecode stile :: Either String SWrap)
  let dg = buildFinalDiagram' <$> layy <*> t
  either putStrLn writeSvg dg

renderStyleSVG :: Coord -> IO B.ByteString
renderStyleSVG c = do
  fp <- getCurrentDirectory
  t <- getMTTile c
  stile <- B.readFile $ fp <> "/poc_style.json"
  let layy = tlayers <$> (A.eitherDecode stile :: Either String SWrap)
  let dg = buildFinalDiagram' <$> layy <*> t :: Either String (D.Diagram D.B)
  either
    (return . B.fromStrict . BC.pack)
    ( return
        . renderBS
        . D.renderDia
          D.SVG
          (D.SVGOptions (D.mkWidth 512) Nothing "" [] True)
    )
    dg

renderStyleSpec :: IO ()
renderStyleSpec = do
  fp <- getCurrentDirectory
  t <- fakerTile
  stile <- B.readFile "/Users/flex99/map/styles/dataviz.json"
  let layy = tlayers <$> (A.eitherDecode stile :: Either String SWrap)
  let dg = buildFinalDiagram' <$> layy <*> t
  either putStrLn writeSvg dg

renderWithMultiple :: IO ()
renderWithMultiple = do
  contours <- getFromUrl "https://api.maptiler.com/tiles/contours-v2/14/8732/5788.pbf?key=1x4mQ1VBiPXKbmnR1S8p"
  outdoor <- getFromUrl "https://api.maptiler.com/tiles/outdoor/14/8732/5788.pbf?key=1x4mQ1VBiPXKbmnR1S8p"
  base <- getFromUrl "https://api.maptiler.com/tiles/v3-openmaptiles/14/8732/5788.pbf?key=1x4mQ1VBiPXKbmnR1S8p"
  stile <- B.readFile "/home/flex99/styles/outdoor.json"
  let layy = tlayers <$> (A.eitherDecode stile :: Either String SWrap)
  let dcontour = buildFinalDiagram' <$> layy <*> contours
  let doutdoor = buildFinalDiagram' <$> layy <*> outdoor
  let dbase = buildFinalDiagram' <$> layy <*> base
  let dg = foldl1 (\x y -> D.atop <$> x <*> y) [dcontour, dbase, doutdoor]
  either putStrLn writeSvg dg
