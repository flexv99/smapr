{-# LANGUAGE RankNTypes #-}

module Renderer.Lines
  ( lineToPoints,
    drawLine,
  )
where

import Control.Lens
import Decoder.Geometry
import Decoder.Lines
import qualified Diagrams.Backend.SVG as D
import qualified Diagrams.Prelude as D
import Style.ExpressionsContext
import Style.IsoExpressions
import Style.Layers.Line
import Style.Parser
import Util

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
lineToPoints (LineG lMoveTo lLineTo) = toDPoint $ _parameters lMoveTo ++ _parameters lLineTo
  where
    toDPoint = map geometryPointToDPoint

drawLine :: LineS -> ExpressionContext -> [D.P2 Double] -> D.Diagram D.B
drawLine style ctx tour =
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
    color = eval (style ^. lineColor) ctx
    stroke = numToDouble $ eval (style ^. lineWidth) ctx
    tourPath = D.fromVertices tour :: D.Trail' D.Line D.V2 Double
