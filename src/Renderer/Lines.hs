{-# LANGUAGE OverloadedStrings #-}

module Renderer.Lines where

import Graphics.Svg
import qualified Data.Text as T
import Util
import ApiClient
import Decoder.Geometry
import Proto.Vector_tile.Tile (Tile(..))

svg :: Element -> Element
svg content =
  doctype
  <> with (svg11_ content) [Version_ <<- "1.1", Width_ <<- "100%", Height_ <<- "100%"]

renderCommands :: [Geometry] -> Element
renderCommands g =
  path_ [
  Fill_     <<- "None"
  , Stroke_ <<- "#ffffff"
  , D_      <<- ( T.intercalate " " $ map (geoToSvgPath) g )
        ]

geoToSvgPath :: Geometry -> T.Text
geoToSvgPath g = case geometryCommand g of
  MoveTo    -> T.intercalate " " $ map (\(x, y) -> mA x y) (parameters g)
  LineTo    -> T.intercalate " " $ map (\(x, y) -> lA x y) (parameters g)
  ClosePath -> z


testSvg :: IO (Maybe Element)
testSvg = do
  tile <- getTile testCoord
  let features = concat <$> map (decodeCommands . map fromIntegral) <$> tileFeatures <$> tile
  return $ svg <$> renderCommands <$> features
  
