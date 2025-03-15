{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Renderer.Polygons (
  polygonToPoints,
  decPolygon,
  drawPolygon,
)
where

import Control.Lens
import Control.Monad.Reader
import Data.Colour
import Data.Maybe (fromMaybe, maybe)
import Data.Scientific (toRealFloat)
import Decoder.Polygons
import qualified Diagrams.Prelude as D hiding (difference)
import qualified Diagrams.TwoD.Path.Boolean as D
import Style.ExpressionsContext
import Style.Lang.Eval
import Style.Lang.Parser
import Style.Lang.Util
import Style.Layers.Fill

polygonToPoints :: SPolygon -> [D.P2 Double]
polygonToPoints (SPolygon moveTo lineTo closeP) = _parameters moveTo ++ _parameters lineTo ++ _parameters closeP

drawPolygon
  :: forall {b}
   . (D.Renderable (D.Path D.V2 Double) b)
  => FillS
  -> [PolygonG]
  -> Reader ExpressionContext (D.QDiagram b D.V2 Double D.Any)
drawPolygon style tour = do
  mColor <- eval (style ^. fillColor)
  let color = maybe black pureColor mColor
  mOpacity <- eval (style ^. fillOpacity)
  let opacity = maybe 1.0 toRealFloat mOpacity
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

singleToPoints :: SPolygon -> D.Located (D.Trail' D.Loop D.V2 Double)
singleToPoints (SPolygon moveTo lineTo closeP) =
  D.fromVertices $
    _parameters moveTo
      ++ _parameters lineTo
      ++ _parameters closeP

multipleToPoints :: MPolygon -> D.Path D.V2 Double
multipleToPoints (o, i) = D.difference D.EvenOdd (D.toPath (singleToPoints o)) (D.toPath (D.loopUnion 0.1 D.EvenOdd $ map singleToPoints i))

polygonToLoop :: PolygonG -> D.Path D.V2 Double
polygonToLoop (SinglePolygon s) = D.toPath $ singleToPoints s
polygonToLoop (MultiPolygon m) = multipleToPoints m

-- >> writeSvg $ drawPolygon'

{-
difference seems to do the trick still need to apply it to located loops
diffEx = (D.strokePath $ D.difference D.Winding (D.square 1) $ D.circle 0.3) D.# D.fc D.purple
-}
