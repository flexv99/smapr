module Renderer.Geometry where

import Control.Lens
import Data.Foldable
import qualified Data.Sequence as S
import qualified Data.Text.Lazy as T
import Decoder.Geometry
import qualified Diagrams.Backend.SVG as D
import qualified Diagrams.Prelude as D
import GHC.Word
import Proto.Util
import Proto.Vector_tile.Tile
import Proto.Vector_tile.Tile.Feature
import Proto.Vector_tile.Tile.GeomType
import Proto.Vector_tile.Tile.Layer
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

featureToDiagram :: Paint -> ExpressionContext -> D.Diagram D.B
featureToDiagram (LinePaint l) ctx = foldl1 D.atop $ map (drawLine l ctx . lineToPoints) (decode' (geometry (ctx ^. feature)) :: [LineG])
featureToDiagram (FillPaint f) ctx = foldl1 D.atop $ map (drawPolygon f ctx . polygonToPoints) (decode' (geometry (ctx ^. feature)) :: [PolygonG])

decode' :: (MapGeometry a) => S.Seq Word32 -> [a]
decode' g = decode $ map fromIntegral $ toList g

renderLayer :: Paint -> S.Seq ExpressionContext -> D.Diagram D.B
renderLayer _ S.Empty = D.strutX 0
renderLayer style f = D.reflectY (foldl1 D.atop $ fmap (featureToDiagram style) f)

-- TODO fix zoom
constructCtx :: S.Seq Layer -> S.Seq ExpressionContext
constructCtx (l S.:<| xs) = create l S.>< constructCtx xs
  where
    create :: Layer -> S.Seq ExpressionContext
    create l' = fmap (\f -> ExpressionContext f l' 17) (features l')
constructCtx S.Empty = S.empty

toBeDrawn :: Tile -> SLayer -> S.Seq ExpressionContext
toBeDrawn t s = fmap (S.filter (evalLayer s)) constructCtx layers'
  where
    layers' = maybe S.empty (`getLayers` t) (s ^. sourceLayer)

-- TODO delegate atop between different source layers here.
