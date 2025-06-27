{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Style.Poc where

import ApiClient
import Control.Monad (join)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as B
import Data.Colour.SRGB
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text.Lazy as T
import Decoder.Geometry
import Decoder.Lines
import qualified Diagrams.Backend.SVG as D
import qualified Diagrams.Prelude as D
import GHC.Generics
import Lens.Micro
import Proto.Util
import Proto.Vector
import Renderer.Geometry
import Style.Lang.Util
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
      fromMaybe
        (sRGB24 232 229 216)
        ( pureColor
            <$> join (fmap (\x -> renderBg x t) (firstLayerByType l))
        )
    renderLayers' ls = mconcat (map (renderTile t) ls)
    firstLayerByType = listToMaybe . filter (\x -> x ^. pType == "background")
    splitted = split' l

pLayer :: IO (Either String SWrap)
pLayer = B.readFile "/home/flex99/tmp/osm.json" <&> A.eitherDecode

renderStyleSpec :: IO ()
renderStyleSpec = do
  t <- fakerTile -- fakerFromP
  stile <- B.readFile "/Users/felixvalentini/tmp/street1.json"
  let layy = tlayers <$> (A.eitherDecode stile :: Either String SWrap)
  let dg = buildFinalDiagram' <$> layy <*> t
  either putStrLn writeSvg dg
  where
    fakerFromP = B.readFile "/Users/felixvalentini/tmp/montebelluna.pbf" >>= return . transformRawTile

renderWithCoords :: Coord -> IO ()
renderWithCoords coord = do
  t <- getMTTile coord
  stile <- B.readFile "/Users/felixvalentini/tmp/street1.json"
  let layy = tlayers <$> (A.eitherDecode stile :: Either String SWrap)
  let dg = buildFinalDiagram' <$> layy <*> t
  either putStrLn writeSvg dg

renderBIG :: Coord -> IO ()
renderBIG coord = do
  ts <- getFiveTiles coord
  stile <- B.readFile "/Users/flex99/tmp/gta.json"
  let layy = uwrap $ tlayers <$> (A.decode stile :: Maybe SWrap)
  let dg = map (\t -> buildFinalDiagram' layy t) <$> ts
  either putStrLn writeSvg (renderFromList <$> dg)
  where
    uwrap (Just x) = x
    renderFromList (hl : hm : hr : ml : mm : mr : ll : lm : lr : []) = (hl D.||| hm D.||| hr) D.=== (ml D.||| mm D.||| mr) D.=== (ll D.||| lm D.||| lr)

debugRenderer :: Coord -> IO ()
debugRenderer c = do
  t <- getMTTile c
  stile <- B.readFile "/Users/felixvalentini/tmp/street1.json"
  let layy = tlayers <$> (A.eitherDecode stile :: Either String SWrap)
  let dg = buildFinalDiagram' <$> layy <*> (filterLayers "transportation" <$> t)
  either putStrLn writeSvg dg

drawTour :: [D.P2 Double] -> D.Diagram D.B
drawTour tour = tourPoints <> D.strokeP tourPath
  where
    tourPath = D.fromVertices tour
    tourPoints = D.atPoints (concat . D.pathVertices $ tourPath) (repeat dot)
    dot = D.circle 0.05 D.# D.fc D.black
