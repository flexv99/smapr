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
import Data.Monoid
import Data.Scientific (toRealFloat)
import qualified Data.Text.Lazy as T
import Decoder.Geometry
import Decoder.Helper
import Decoder.Points
import qualified Diagrams.Prelude as D
import qualified Diagrams.TwoD.Text as D
import Lens.Micro
import Style.ExpressionsContext
import Style.Lang.Eval
import Style.Lang.Parser
import Style.Layers.Point

pPointToPoints :: PointG -> [D.P2 Double]
pPointToPoints p = p ^. parameters

drawPoint
  :: ( D.Renderable (D.Path D.V2 Double) b
     , D.Renderable (D.Text Double) b
     )
  => PointS -> [D.P2 Double] -> Reader ExpressionContext (D.QDiagram b D.V2 Double D.Any)
drawPoint style tour = do
  text <- eval (style ^. textField)
  -- mColor <- eval (style ^. lineColor)
  -- let color = fromMaybe (D.black `D.withOpacity` 1.0) mColor
  -- mStroke <- eval (style ^. lineWidth)
  -- let strokeWidth = maybe 1.0 realToFrac mStroke
  pure $
    mconcat $
      map
        ( \tr ->
            D.circle 1
              D.# D.fc black
              D.# D.moveTo tr
              <> case T.unpack <$> text of
                Just t -> D.text "text" D.# D.fontSize (D.local 0.5) D.# D.moveTo tr
                Nothing -> mempty
        )
        tour
