{-# LANGUAGE OverloadedStrings #-}

module Renderer.Lines where

import Graphics.Svg
import Util
import Decoder.Geometry
import qualified Data.Text as T

svg :: Element -> Element
svg content =
  doctype
  <> with (svg11_ content) [Version_ <<- "1.1", Width_ <<- "100%", Height_ <<- "100%"]

contents :: Element
contents =
  path_ [
  Fill_     <<- "None"
  , Stroke_ <<- "#352950"
  , D_      <<- ( mA 0 340 <> lA 113 170 <> lA 0 0 <> lA 85 0
                  <> lA 198 170 <> lA 85 340 <> lA 0 340 <> z <> mA 0 340 ) ]

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
