{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Renderer.Polygons
  ( polygonToPoints,
    drawPolygon,
  )
where

import Control.Lens
import Data.Colour
import Decoder.Polygons
import qualified Diagrams.Backend.SVG as D
import qualified Diagrams.Prelude as D
import Style.ExpressionsContext
import Style.IsoExpressions
import Style.Layers.Fill
import Style.Parser

geoMetryPointToDPoint :: Point -> D.P2 Double
geoMetryPointToDPoint (x, y) = x D.^& y

polygonToPoints :: PolygonG -> [D.P2 Double]
polygonToPoints (PolygonG moveTo lineTo closeP) = toDPoint $ _parameters moveTo ++ _parameters lineTo ++ _parameters closeP
  where
    toDPoint = map geoMetryPointToDPoint

drawPolygon :: FillS -> ExpressionContext -> [D.P2 Double] -> D.Diagram D.B
drawPolygon style ctx tour =
  D.strokeLocLoop tourPath
    D.# D.fcA color
    D.# D.lcA color
  where
    color = pureColor (eval (style ^. fillColor) ctx) `withOpacity` (numToDouble (eval (style ^. fillOpacity) ctx))
    tourPath = D.fromVertices tour
