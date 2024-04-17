{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Renderer.Polygons () where

import Diagrams.Prelude
import Diagrams.TwoD.Size
import Diagrams.Backend.SVG
import Util

myCircle :: Diagram B
myCircle = circle 1


renderTile :: IO ()
renderTile = do
  let sz = mkSizeSpec2D (Just 512) (Just 512)
  dateString <- dateTimeStr
  path <- testPath dateString
  putStrLn path
  renderSVG path sz myCircle
