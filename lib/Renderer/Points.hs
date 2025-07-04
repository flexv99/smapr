{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Renderer.Points (
  pPointToPoints,
  drawPoint,
)
where

import Control.Monad.Reader
import Data.Maybe (fromMaybe)
import Data.Scientific (toRealFloat)
import qualified Data.Text.Lazy as T
import Decoder.Helper
import qualified Diagrams.Prelude as D
import qualified Diagrams.TwoD.Text as D
import Lens.Micro
import Style.ExpressionsContext
import Style.Lang.Eval
import Style.Lang.Util (pureColor)
import Style.Layers.Point

pPointToPoints :: PointG -> [D.P2 Double]
pPointToPoints p = p ^. parameters

drawPoint
  :: ( D.Renderable (D.Path D.V2 Double) b
     , D.Renderable (D.Text Double) b
     )
  => PointS -> PointS -> [D.P2 Double] -> Reader ExpressionContext (D.QDiagram b D.V2 Double D.Any)
drawPoint paint layout tour = do
  text <- eval (layout ^. textField)
  size <- eval (layout ^. textSize)
  mRotationDegrees <- eval (layout ^. textRotate)
  let rotationDeg = fromMaybe (0) mRotationDegrees
  mColor <- eval (paint ^. textColor)
  let color = fromMaybe (D.black `D.withOpacity` 1.0) mColor
  -- mStroke <- eval (style ^. lineWidth)
  -- let strokeWidth = maybe 1.0 realToFrac mStroke
  pure $
    mconcat $
      map
        ( \tr ->
            case T.unpack <$> text of
              Just t ->
                if not $ null t
                  then
                    ptD
                      tr
                      t
                      (toRealFloat <$> size)
                      (pureColor color)
                      (toRealFloat rotationDeg)
                  else mempty
              Nothing -> mempty
        )
        tour
  where
    degreesToFloat deg = (deg `mod'` 360) / 360
      where
        mod' a b = a - fromIntegral (floor (a / b)) * b
    ptD r t s c d =
      D.text t
        D.# D.fontSize (D.local (fromMaybe 16 s))
        D.# D.moveTo r
        D.# D.fc c
        D.# D.reflectAbout r (D.rotateBy (0.5 + d) D.xDir)
