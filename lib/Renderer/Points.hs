{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Renderer.Points (
  pPointToPoints,
  drawPoint,
)
where

import Control.Monad.Reader
import Data.Colour
import Data.Maybe (fromMaybe)
import Data.Scientific (toRealFloat)
import Decoder.Geometry
import Decoder.Helper
import Decoder.Points
import qualified Diagrams.Prelude as D
import Lens.Micro
import Style.ExpressionsContext
import Style.Lang.Eval
import Style.Lang.Parser
import Style.Layers.Line

pPointToPoints :: PointG -> [D.P2 Double]
pPointToPoints (PointG p) = p ^. parameters

drawPoint
  :: forall {b}
   . (D.Renderable (D.Path D.V2 Double) b)
  => LineS
  -> [D.P2 Double]
  -> Reader ExpressionContext (D.QDiagram b D.V2 Double D.Any)
drawPoint style tour = do
  -- mColor <- eval (style ^. lineColor)
  -- let color = fromMaybe (black `withOpacity` 1.0) mColor
  -- mStroke <- eval (style ^. lineWidth)
  -- let stroke = maybe 1.0 toRealFloat mStroke
  return $ D.moveTo (head tour) (D.circle 0.07)
