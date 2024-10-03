{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Renderer.Polygons
  ( polygonToPoints,
    decPolygon,
    drawPolygon,
  )
where

import Control.Lens
import Control.Monad.Reader
import Data.Bifunctor
import Data.Colour
import Decoder.Polygons
import qualified Diagrams.Backend.SVG as D
import qualified Diagrams.Prelude as D hiding (difference)
import qualified Diagrams.TwoD.Path.Boolean as D
import Style.ExpressionsContext
import Style.IsoExpressions
import Style.Layers.Fill
import Style.Parser
import Util

geoMetryPointToDPoint :: Point -> D.P2 Double
geoMetryPointToDPoint (x, y) = x D.^& y

polygonToPoints :: SPolygon -> [D.P2 Double]
polygonToPoints (SPolygon moveTo lineTo closeP) = toDPoint $ _parameters moveTo ++ _parameters lineTo ++ _parameters closeP
  where
    toDPoint = map geoMetryPointToDPoint

drawPolygon ::
  forall {b}.
  (D.Renderable (D.Path D.V2 Double) b) =>
  FillS ->
  [PolygonG] ->
  Reader ExpressionContext (D.QDiagram b D.V2 Double D.Any)
drawPolygon style tour = do
  color <- fmap pureColor (eval (style ^. fillColor))
  opacity <- fmap numToDouble (eval (style ^. fillOpacity))
  return $
    mconcat $
      map
        ( \t ->
            D.strokeP (polygonToLoop t)
              D.# D.fcA (color `withOpacity` opacity)
              D.# D.lcA (color `withOpacity` opacity)
              D.# D.lwG 0
              D.# D.fillRule D.Winding
        )
        tour

testInner :: [PolygonG]
testInner = decPolygon [9, 0, 0, 26, 20, 0, 0, 20, 19, 0, 15, 9, 22, 2, 26, 18, 0, 0, 18, 17, 0, 15, 9, 4, 13, 26, 0, 8, 8, 0, 0, 7, 15]

singleToPoints :: SPolygon -> D.Located (D.Trail' D.Loop D.V2 Double)
singleToPoints (SPolygon moveTo lineTo closeP) =
  D.fromVertices $
    toDPoint $
      _parameters moveTo
        ++ _parameters lineTo
        ++ _parameters closeP
  where
    toDPoint = map geoMetryPointToDPoint

multipleToPoints :: MPolygon -> D.Path D.V2 Double
multipleToPoints (o, i) = D.difference D.EvenOdd (D.toPath (singleToPoints o)) (D.toPath (D.loopUnion 0.1 D.EvenOdd $ map singleToPoints i))

polygonToLoop :: PolygonG -> D.Path D.V2 Double
polygonToLoop (SinglePolygon s) = D.toPath $ singleToPoints s
polygonToLoop (MultiPolygon m) = multipleToPoints m

drawPolygon' =
  mconcat $
    map
      ( \t ->
          D.strokeP (polygonToLoop t)
            D.# D.fc D.purple
            D.# D.lc D.blue
            D.# D.lwG 0
      )
      testInner

-- >> writeSvg $ drawPolygon'

{-
difference seems to do the trick still need to apply it to located loops
diffEx = (D.strokePath $ D.difference D.Winding (D.square 1) $ D.circle 0.3) D.# D.fc D.purple
-}
