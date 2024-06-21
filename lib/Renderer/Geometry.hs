module Renderer.Geometry (test) where

import ApiClient
import Data.Foldable
import qualified Data.Sequence as S
import Decoder.Geometry
import qualified Diagrams.Backend.SVG as D
import qualified Diagrams.Prelude as D
import qualified Diagrams.Trail as D
import qualified Diagrams.TwoD.Size as D
import GHC.Word
import Proto.Vector_tile.Tile
import Proto.Vector_tile.Tile.Feature
import Proto.Vector_tile.Tile.GeomType
import Proto.Vector_tile.Tile.Layer
import Renderer.Lines
import Renderer.Polygons
import Util

drawTour :: [D.P2 Double] -> D.Diagram D.B
drawTour tour = tourPoints <> D.strokeP tourPath
  where
    tourPath = D.fromVertices tour
    tourPoints = D.atPoints (concat . D.pathVertices $ tourPath) (repeat dot)
    dot = D.circle 0.05 D.# D.fc D.black

featureToDiagram :: Feature -> D.Diagram D.B
featureToDiagram (Feature _ _ (Just POLYGON) g) = foldl1 D.atop $ map (drawTour . polygonToPoints) (decode' g :: [PolygonG])
featureToDiagram (Feature _ _ (Just LINESTRING) g) = foldl1 D.atop $ map (drawTour . lineToPoints) (decode' g :: [LineG])
featureToDiagram _ = D.strutX 0

decode' :: (MapGeometry a) => S.Seq Word32 -> [a]
decode' g = decode $ map fromIntegral $ toList g

renderLayer :: String -> Tile -> D.Diagram D.B
renderLayer l t = D.reflectY . foldl1 D.atop . map featureToDiagram . head . map toList . (map features <$> toList) $ getLayers l t

test :: IO ()
test = do
  t <- fakerTile
  let roads = renderLayer "transportation" <$> t
  let buildings = renderLayer "building" <$> t
  let d = fmap (<>) roads <*> buildings
  maybe (putStrLn "Nothing") writeSvg d
