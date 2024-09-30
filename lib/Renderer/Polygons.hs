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

-- todo check isInner here
polygonToPoints :: SPolygon -> [D.P2 Double]
polygonToPoints (SPolygon moveTo lineTo closeP) = toDPoint $ _parameters moveTo ++ _parameters lineTo ++ _parameters closeP
  where
    toDPoint = map geoMetryPointToDPoint

drawPolygon ::
  forall {b}.
  (D.Renderable (D.Path D.V2 Double) b) =>
  FillS ->
  [D.P2 Double] ->
  Reader ExpressionContext (D.QDiagram b D.V2 Double D.Any)
drawPolygon style tour = do
  color <- fmap pureColor (eval (style ^. fillColor))
  opacity <- fmap numToDouble (eval (style ^. fillOpacity))
  return
    ( D.strokeLocLoop tourPath
        D.# D.fcA (color `withOpacity` opacity)
        D.# D.lcA (color `withOpacity` opacity)
        D.# D.lwG 0
        D.# D.fillRule D.Winding
    )
  where
    tourPath = D.fromVertices tour

testInner :: PolygonG
testInner = decPolygon [9, 0, 0, 26, 20, 0, 0, 20, 19, 0, 15, 9, 22, 2, 26, 18, 0, 0, 18, 17, 0, 15, 9, 4, 13, 26, 0, 8, 8, 0, 0, 7, 15]

testS :: [SPolygon]
testS = helperDecSPolygon [9, 0, 0, 26, 20, 0, 0, 20, 19, 0, 15, 9, 22, 2, 26, 18, 0, 0, 18, 17, 0, 15, 9, 4, 13, 26, 0, 8, 8, 0, 0, 7, 15]

testOld :: IO ()
testOld = writeSvg $ mconcat $ map ((D.strokeLocLoop . D.fromVertices) . polygonToPoints) testS

polygonToPoints' :: PolygonG -> Either [D.P2 Double] [([D.P2 Double], [[D.P2 Double]])]
polygonToPoints' (SinglePolygon s) = Left $ polygonToPoints s
polygonToPoints' (MultiPolygon m) = Right $ map (bimap polygonToPoints (map polygonToPoints)) m

polyAtop ::
  (D.Renderable (D.Path D.V2 Double) b) =>
  Either [D.P2 Double] [([D.P2 Double], [[D.P2 Double]])] ->
  D.QDiagram b D.V2 Double D.Any
polyAtop (Left a) = D.strokeLocLoop $ D.fromVertices a
polyAtop (Right r) = mconcat $ map (\(a, b) -> D.strokeLocLoop (D.fromVertices a) <> mconcat (map (D.strokeLocLoop . D.reverseLocLoop . D.fromVertices) b)) r D.# D.fc D.purple D.# D.fillRule D.EvenOdd

-- multiPolyAtop (Interior i) (Exterior e) = Exterior (e <> D.reverseLocLoop i D.# D.fc D.red)
-- multiPolyAtop (Exterior e) (Interior i) = Exterior (e <> D.reverseLocLoop i D.# D.fc D.red)
-- multiPolyAtop (Exterior e) (Exterior i) = Exterior (i <> e D.# D.fc D.red)
-- multiPolyAtop (Interior e) (Interior i) = Exterior (i <> e D.# D.fc D.red)

-- -- testRender :: MultiPoint (D.QDiagram b D.V2 Double D.Any)
-- testRender = foldl1 multiPolyAtop (map delegate pts)
--   where
--     delegate (Exterior pts) = Exterior $ D.fromVertices pts
--     delegate (Interior pts) = Interior $ D.fromVertices pts

-- render2DVector :: D.Diagram D.B -> IO ()
-- render2DVector v = do
--   let sz = D.mkSizeSpec2D (Just 512) (Just 512)
--   dateStr <- dateTimeStr
--   path <- testPath dateStr
--   putStrLn path
--   D.renderSVG path sz $ v D.# D.fillRule D.EvenOdd
