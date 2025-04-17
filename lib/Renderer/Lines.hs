{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Renderer.Lines (
  lineToPoints,
  drawLine,
)
where

import Control.Monad.Reader
import Data.Colour
import Data.Maybe (fromMaybe)
import Data.Scientific (toRealFloat)
import Decoder.Geometry
import Decoder.Lines
import qualified Diagrams.Prelude as D
import Lens.Micro
import Style.ExpressionsContext
import Style.Lang.Eval
import Style.Lang.Parser
import Style.Layers.Line

lineToPoints :: LineG -> [D.P2 Double]
lineToPoints (LineG lMoveTo lLineTo) = _parameters lMoveTo ++ _parameters lLineTo

drawLine
  :: forall {b}
   . (D.Renderable (D.Path D.V2 Double) b)
  => LineS
  -> [D.Point D.V2 Double]
  -> Reader ExpressionContext (D.QDiagram b D.V2 Double D.Any)
drawLine style tour = do
  mColor <- eval (style ^. lineColor)
  let color = fromMaybe (black `withOpacity` 1.0) mColor
  mStroke <- eval (style ^. lineWidth)
  let stroke = maybe 1.0 toRealFloat mStroke
  return $
    D.moveTo
      (head tour)
      ( tourPath
          D.# D.strokeLine
          D.# D.lcA color
          D.# D.lwG stroke
          D.# lineP
      )
  where
    lineP :: forall {c}. (D.HasStyle c) => c -> c
    lineP = D.lineCap (style ^. lineCap) . D.lineJoin (style ^. lineJoin)
    tourPath = D.fromVertices tour :: D.Trail' D.Line D.V2 Double
