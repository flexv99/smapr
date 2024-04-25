{-# LANGUAGE OverloadedStrings #-}

module Renderer.Lines
  ( lineToPoints
  ) where

import Control.Monad
import qualified Data.Text as T
import qualified Diagrams.Prelude as D
import qualified Diagrams.TwoD.Size as D
import qualified Diagrams.Backend.SVG as D
import qualified Diagrams.Trail as D
import Util
import ApiClient
import Decoder.Geometry
import Decoder.Lines
import Proto.Vector_tile.Tile (Tile(..))

render2DVector :: D.Diagram D.B -> IO ()
render2DVector v = do
  let sz = D.mkSizeSpec2D (Just 512) (Just 512)
  dateStr <- dateTimeStr
  path <- testPath dateStr
  putStrLn path
  D.renderSVG path sz $ v D.# D.showOrigin

geometryPointToDPoint :: Point -> D.P2 Double
geometryPointToDPoint (x, y) = x D.^& y 

lineToPoints :: LineG -> [D.P2 Double]
lineToPoints (LineG lMoveTo lLineTo) = toDPoint $ parameters lMoveTo ++ parameters lLineTo
  where
    toDPoint = map geometryPointToDPoint
