{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Renderer.Polygons () where

import qualified Diagrams.Prelude as D
import qualified Diagrams.TwoD.Size as D
import qualified Diagrams.Backend.SVG as D
import qualified Diagrams.Trail as D
import Proto.Vector_tile.Tile (Tile(..))
import Util
import ApiClient
import Decoder.Geometry
import Decoder.Polygons

render2DVector :: D.Diagram D.B -> IO ()
render2DVector v = do
  let sz = D.mkSizeSpec2D (Just 512) (Just 512)
  dateStr <- dateTimeStr
  path <- testPath dateStr
  putStrLn path
  D.renderSVG path sz $ v D.# D.showOrigin

geoMetryPointToDPoint :: Point -> D.P2 Double
geoMetryPointToDPoint (x, y) = x D.^& y

drawTour :: [Point] -> D.Diagram D.B
drawTour tour = tourPoints <> D.strokeP tourPath
  where
    tourPath   = D.fromVertices . map geoMetryPointToDPoint $ tour
    tourPoints = D.atPoints (concat . D.pathVertices $ tourPath) (repeat dot)
    dot = D.circle 0.05 D.# D.fc D.black

diagramFromFeature :: Proto.Vector_tile.Tile.Tile
                   -> String -> D.QDiagram D.B D.V2 Double D.Any
diagramFromFeature t f = foldr1 D.atop (map (drawTour . polygonToPoints) roadGeo)
  where
    roadGeo = concatMap (decode . map fromIntegral) $ filterLayerByName f t

renderPath :: IO ()
renderPath = do
  t <- fakerTile
  let f = concatMap (decode . map fromIntegral) . filterLayerByName "roads" <$> t
  let tbDrawn = map polygonToPoints <$> f
  let path = foldr1 D.atop . map drawTour <$> tbDrawn
  maybe (putStrLn "nothing") render2DVector path


polygonToPoints :: PolygonG -> [Point]
polygonToPoints p = parameters (pMoveTo p) ++ parameters (pLineTo p) ++ parameters (pClosePath p)

testMultipleLayers :: IO ()
testMultipleLayers = do
  t <- fakerTile
  let roads = fmap (`diagramFromFeature` "roads") t
  let buildings = fmap (`diagramFromFeature` "buildings") t
  maybe (putStrLn "nothing") render2DVector (fmap (<>) roads <*> buildings)
