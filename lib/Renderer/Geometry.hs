{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Renderer.Geometry where

import Control.Monad
import Control.Monad.Reader
import Data.Foldable
import Data.Maybe (fromMaybe)
import qualified Data.Sequence as S
import Decoder.Geometry
import qualified Diagrams.Prelude as D
import Lens.Micro
import Proto.Util
import Renderer.Lines
import Renderer.Polygons
import Style.ExpressionsContext
import Style.Layers.Wrapper

{-
fromVertices returns an instance of TrailLike
which can be:  lines, loops, trails, paths, vertex lists, and diagrams.

tourPath is ment to draw lines (what a suprise!)
we explicitly cast to a line: D.Trail' D.Line D.V2 Double
on this type we can then apply our line appearence properties

moveTo will determine where the origin is set
-}

featureToDiagram
  :: forall {b}
   . (D.Renderable (D.Path D.V2 Double) b)
  => Maybe Paint
  -> Reader ExpressionContext (D.QDiagram b D.V2 Double D.Any)
featureToDiagram (Just (LinePaint l)) = do
  linePath <- decode' :: Reader ExpressionContext [LineG]
  mconcat <$> mapM (drawLine l . lineToPoints) linePath
featureToDiagram (Just (FillPaint f)) = do
  polygonPath <- decode' :: Reader ExpressionContext [PolygonG]
  drawPolygon f polygonPath
featureToDiagram _ = return $ D.strutX 0

decode' :: (MapGeometry a, Show a) => Reader ExpressionContext [a]
decode' = ask >>= \ctx -> return $ decodeSeq (geometry (ctx ^. feature))

renderTile
  :: forall {b}
   . (D.Renderable (D.Path D.V2 Double) b)
  => Tile
  -> SLayer
  -> D.QDiagram b D.V2 Double D.Any
renderTile tile layer' = do
  D.reflectY $ mconcat $ map eachLayer (toList $ constructCtx layers')
  where
    toBeDrawn = runReader (evalLayer layer')
    eachLayer ctx = if fromMaybe True (toBeDrawn ctx) then runReader (featureToDiagram (layer' ^. paint)) ctx else D.strutX 0
    layers' =
      maybe
        S.empty
        (`getLayers` tile)
        (layer' ^. sourceLayer)

-- TODO fix zoom
constructCtx :: S.Seq Layer -> S.Seq ExpressionContext
constructCtx (l S.:<| xs) = create l S.>< constructCtx xs
  where
    create :: Layer -> S.Seq ExpressionContext
    create l' = fmap (\f -> ExpressionContext f l' 17) (features l')
constructCtx S.Empty = S.empty
