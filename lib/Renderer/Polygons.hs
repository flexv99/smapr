{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Renderer.Polygons
  ( polygonToPoints
  , drawPolygon
  ) where

import qualified Diagrams.Prelude as D
import qualified Diagrams.TwoD.Size as D
import qualified Diagrams.Backend.SVG as D
import qualified Diagrams.Trail as D
import Proto.Vector_tile.Tile (Tile(..))
import Control.Lens
import Util
import ApiClient
import Decoder.Polygons
import Style.Layers.Fill
import Style.ExpressionsContext
import Style.ExpressionsEval

render2DVector :: D.Diagram D.B -> IO ()
render2DVector v = do
  let sz = D.mkSizeSpec2D (Just 512) (Just 512)
  dateStr <- dateTimeStr
  path <- testPath dateStr
  putStrLn path
  D.renderSVG path sz $ v D.# D.showOrigin

geoMetryPointToDPoint :: Point -> D.P2 Double
geoMetryPointToDPoint (x, y) = x D.^& y

polygonToPoints :: PolygonG -> [D.P2 Double]
polygonToPoints (PolygonG moveTo lineTo closeP) = toDPoint $ _parameters moveTo ++ _parameters lineTo ++ _parameters closeP
  where
    toDPoint = map geoMetryPointToDPoint

drawPolygon :: FillS -> ExpressionContext -> [D.P2 Double] -> D.Diagram D.B
drawPolygon style ctx tour = D.moveTo (head tour)
  (D.strokeP tourPath D.# D.fcA color) 
  where
    color = unwrapSColor (style ^. fillColor)
    tourPath = D.fromVertices tour

