module Renderer.Geometry where

import qualified Data.Sequence as S
import qualified Diagrams.Backend.SVG as D
import qualified Diagrams.Prelude as D
import qualified Diagrams.Trail as D
import qualified Diagrams.TwoD.Size as D
import qualified Diagrams.Located as D
import GHC.Word
import Data.Foldable
import Proto.Vector_tile.Tile
import Proto.Vector_tile.Tile.Feature
import Proto.Vector_tile.Tile.GeomType
import Proto.Vector_tile.Tile.Layer
import Renderer.Lines
import Renderer.Polygons
import Decoder.Geometry
import ApiClient
import Util

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

drawLine :: [D.P2 Double] -> D.Diagram D.B
drawLine tour = D.moveTo (head tour) (tourPath D.# D.strokeLine D.# D.lc D.blue D.# D.showOrigin)
  where
    -- Create an unlocated line trail from the vertices
    tourPath = D.fromVertices tour :: D.Trail' D.Line D.V2 Double

featureToDiagram :: Feature -> D.Diagram D.B
featureToDiagram (Feature _ _ (Just POLYGON) g)    = foldl1 D.atop $ map (drawTour . polygonToPoints) (decode' g :: [PolygonG])
featureToDiagram (Feature _ _ (Just LINESTRING) g) = foldl1 D.atop $ map (drawLine . lineToPoints) (decode' g :: [LineG])
featureToDiagram _                                 = D.strutX 0

decode' :: (MapGeometry a) => S.Seq Word32 -> [a]
decode' g = decode $ map fromIntegral $ toList g

-- Will be deprecated soon
renderLayer :: String -> Tile -> D.Diagram D.B
renderLayer l t = D.reflectY . foldl1 D.atop . map featureToDiagram . head . map toList . (map features <$> toList) $ getLayers l t

renderLayer' :: [Feature] -> D.Diagram D.B
renderLayer' f = D.reflectY $ foldl1 D.atop $ map featureToDiagram f
