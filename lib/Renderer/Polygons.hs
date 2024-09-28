{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Renderer.Polygons
  ( polygonToPoints,
    drawPolygon,
  )
where

import Control.Lens
import Control.Monad.Reader
import Data.Colour
import Data.Typeable
import Decoder.Polygons
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

drawPolygon ::
  forall {b}.
  (D.Renderable (D.Path D.V2 Double) b) =>
  FillS ->
  [D.Point D.V2 Double] ->
  Reader ExpressionContext (D.QDiagram b D.V2 Double D.Any)
drawPolygon style tour = do
  color <- liftM pureColor (eval (style ^. fillColor))
  opacity <- liftM numToDouble (eval (style ^. fillOpacity))
  return
    ( D.strokeLocLoop tourPath
        D.# D.fcA (color `withOpacity` opacity)
        D.# D.lcA (color `withOpacity` opacity)
        D.# D.lwG 0
    )
  where
    tourPath = D.fromVertices tour
