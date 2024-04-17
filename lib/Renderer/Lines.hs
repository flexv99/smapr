{-# LANGUAGE OverloadedStrings #-}

module Renderer.Lines where

import Control.Monad
import qualified Data.Text as T
import Util
import ApiClient
import Decoder.Geometry
import qualified Decoder.Polygon as P
import Proto.Vector_tile.Tile (Tile(..))
import Graphics.Svg

svg :: Element -> Element
svg content =
  doctype
  <> with (svg11_ content) [Version_ <<- "1.1", Width_ <<- "100%", Height_ <<- "100%"]

renderCommands :: [Geometry] -> Element
renderCommands g =
  path_ [
  Fill_           <<- "None"
  , Stroke_       <<- "#ffffff"
  , Stroke_width_ <<- "4"
  , D_            <<- T.intercalate " " ( map geoToSvgPath g )
        ]

renderBuildingCommands :: [Geometry] -> Element
renderBuildingCommands g =
  path_ [
  Fill_           <<- "#f54242"
  , Stroke_       <<- "#ffffff"
  , Stroke_width_ <<- "4"
  , D_            <<- T.intercalate " " ( map geoToSvgPath g )
        ]

geoToSvgPath :: Geometry -> T.Text
geoToSvgPath g = case geometryCommand g of
  MoveTo    -> T.intercalate " " $ map (uncurry mA) (parameters g)
  LineTo    -> T.intercalate " " $ map (uncurry lA) (parameters g)
  ClosePath -> z

testSvg :: IO (Maybe Element)
testSvg = do
  tile <- fakerTile
  let features = concatMap (P.decodeCommands . map fromIntegral) . tileFeatures <$> tile
  return (svg . renderCommands <$> features)


polyTestSvg :: IO (Maybe Element)
polyTestSvg = do
  tile <- getNextzenTile testCoord
  let features = concatMap (P.decodeCommands . map fromIntegral) . filterLayerByName "buildings" <$> tile
  return (svg . renderCommands <$> features)

saveTestSvg :: IO ()
saveTestSvg = testSvg >>= maybe (putStrLn "got nothing") writeSvg
