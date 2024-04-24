module Renderer.Geometry where

import qualified Diagrams.Prelude as D
import qualified Diagrams.TwoD.Size as D
import qualified Diagrams.Backend.SVG as D
import qualified Diagrams.Trail as D
import Data.Foldable
import GHC.Word
import qualified Data.Sequence as S
import Proto.Vector_tile.Tile.Layer
import Proto.Vector_tile.Tile.GeomType
import Proto.Vector_tile.Tile.Feature
import Decoder.Geometry
import Renderer.Polygons
import Renderer.Lines
import Util
import ApiClient

drawTour :: [D.P2 Double] -> D.Diagram D.B
drawTour tour = tourPoints <> D.strokeP tourPath
  where
    tourPath   = D.fromVertices tour
    tourPoints = D.atPoints (concat . D.pathVertices $ tourPath) (repeat dot)
    dot = D.circle 0.05 D.# D.fc D.black

featureToDiagram :: Feature -> D.Diagram D.B
featureToDiagram (Feature _ _ (Just POLYGON) g)    = foldl1 D.atop $ map (drawTour . polygonToPoints) (decode' g :: [PolygonG])
featureToDiagram (Feature _ _ (Just LINESTRING) g) = foldl1 D.atop $ map (drawTour . lineToPoints) (decode' g :: [LineG])

decode' :: (MapGeometry a) => S.Seq Word32 -> [a]
decode' g = decode $ map fromIntegral $ toList g

test :: IO ()
test = do
  let sz = D.mkSizeSpec2D (Just 512) (Just 512)
  dateStr <- dateTimeStr
  path <- testPath dateStr
  putStrLn path
  t <- fakerTile
  let f = head . map toList . (map features <$> toList) . getLayers "roads" <$> t
  let d = foldl1 D.atop . map featureToDiagram <$> f
  maybe (putStrLn "Nothing") (D.renderSVG path sz) d
