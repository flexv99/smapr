module Renderer.Geometry where

import qualified Data.Sequence as S
import qualified Diagrams.Backend.SVG as D
import qualified Diagrams.Prelude as D
import Control.Lens
import Data.Foldable
import Proto.Vector_tile.Tile.Feature
import Proto.Vector_tile.Tile.GeomType
import Style.ExpressionsContext
import Style.Layers.Line
import Renderer.Lines
import Renderer.Polygons
import Decoder.Geometry

drawTour :: [D.P2 Double] -> D.Diagram D.B
drawTour tour = tourPoints <> D.strokeP tourPath
  where
    tourPath = D.fromVertices tour
    tourPoints = D.atPoints (concat . D.pathVertices $ tourPath) (repeat dot)
    dot = D.circle 0.05 D.# D.lc D.blue

{-
fromVertices returns an instance of TrailLike
which can be:  lines, loops, trails, paths, vertex lists, and diagrams.

tourPath is ment to draw lines (what a suprise!)
we explicitly cast to a line: D.Trail' D.Line D.V2 Double
on this type we can then apply our line appearence properties

moveTo will determine where the origin is set
-}

featureToDiagram :: LineS -> ExpressionContext -> D.Diagram D.B
featureToDiagram style ctx = case featureGeometryType ctx of
                          Just LINESTRING -> foldl1 D.atop $ map (drawLine style ctx . lineToPoints) (decode' path :: [LineG])
                          Just POLYGON    -> foldl1 D.atop $ map (drawTour . polygonToPoints) (decode' path :: [PolygonG])
                          _               -> D.strutX 0
  where
    path      = geometry (ctx ^. feature)
    decode' g = decode $ map fromIntegral $ toList g

renderLayer :: LineS -> S.Seq ExpressionContext -> D.Diagram D.B
renderLayer style f = D.reflectY $ foldl1 D.atop $ fmap (featureToDiagram style) f
