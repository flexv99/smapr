{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Renderer.Geometry where

import Control.Monad
import Control.Monad.Reader
import Data.List (uncons)
import Data.Maybe (fromMaybe)
import Decoder.Geometry
import qualified Diagrams.Prelude as D
import Lens.Micro
import Proto.Util
import Proto.Vector
import Proto.Vector_Fields
import Renderer.Lines
import Renderer.Points
import Renderer.Polygons
import Style.ExpressionsContext
import Style.Lang.Eval
import Style.Lang.Types
import Style.Layers.Background
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
featureToDiagram (Just (PointPaint p)) = do
  pointsPath <- decode' :: Reader ExpressionContext [PointG]
  mconcat <$> mapM (drawPoint p . pPointToPoints) pointsPath
featureToDiagram _ = return $ D.strutX 0

decode' :: (MapGeometry a, Show a) => Reader ExpressionContext [a]
decode' = ask >>= \ctx -> return $ decodeVec (ctx ^. (feature . vec'geometry))

renderTile
  :: forall {b}
   . (D.Renderable (D.Path D.V2 Double) b)
  => Tile
  -> SLayer
  -> D.QDiagram b D.V2 Double D.Any
renderTile tile layer' = do
  D.reflectY $ mconcat $ map eachLayer (constructCtx layers')
  where
    toBeDrawn = runReader (evalLayer layer')
    eachLayer ctx =
      if fromMaybe True (toBeDrawn ctx)
        then runReader (featureToDiagram (layer' ^. paint)) ctx
        else D.strutX 0
    layers' =
      maybe
        []
        (`getLayers` tile)
        (layer' ^. sourceLayer)

renderBg :: SLayer -> Tile -> SColor
renderBg l t = join $ bg <$> bgP
  where
    ctx = constructCtx layers'
    bgP = l ^. paint
    bg (BackgroundPaint b) = join $ runReader (eval $ b ^. backgroundColor) . fst <$> (uncons ctx)
    bg _ = error "not a background, shouldn't happen"
    layers' =
      maybe
        []
        (`getLayers` t)
        (l ^. sourceLayer)

-- TODO fix zoom
constructCtx :: [Tile'Layer] -> [ExpressionContext]
constructCtx (l : xs) = create l ++ constructCtx xs
  where
    create :: Tile'Layer -> [ExpressionContext]
    create l' = map (\f -> ExpressionContext f l' 18) (l' ^. features)
constructCtx _ = []
