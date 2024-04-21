{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Renderer.Polygons () where

import qualified Diagrams.Prelude as D
import qualified Diagrams.TwoD.Size as D
import qualified Diagrams.Backend.SVG as D
import qualified Diagrams.Trail as D
import Util
import ApiClient
import Decoder.Geometry
import Decoder.Polygons

-- MoveTo needs pathFromTrailAt from path, which is needed to combine all trails
-- ref: https://hackage.haskell.org/package/diagrams-lib-1.4.6.1/docs/Diagrams-Path.html#v:pathFromTrailAt

-- ClosedPath needs closeLine which needs to know the trail that needs to be closed though
-- ref: https://hackage.haskell.org/package/diagrams-lib-1.4.6.1/docs/Diagrams-Trail.html#v:closeLine

geoToPath :: [GeoAction] -> D.Path D.V2 Double
geoToPath g = start (head g) (mconcat (map (D.wrapTrail . actionForCommand) g))
  where
    actionForCommand :: GeoAction -> D.Trail' D.Line D.V2 Double
    actionForCommand (GeoAction (Command LineTo _) p)    = D.lineFromOffsets $ map D.r2 p
  -- actionForCommand (GeoAction (Command ClosePath _) p) = closeLine p
    actionForCommand _                                  = D.emptyLine
    start :: GeoAction -> D.Trail D.V2 Double -> D.Path D.V2 Double
    start m p = D.pathFromTrailAt p $ D.P $ D.r2 $ head $ parameters m

myCircle :: D.Diagram D.B
myCircle = D.circle 1 `D.atop` D.square (sqrt 2)

renderTile :: IO ()
renderTile = do
  let sz = D.mkSizeSpec2D (Just 512) (Just 512)
  dateString <- dateTimeStr
  path <- testPath dateString
  putStrLn path
  D.renderSVG path sz myCircle

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

renderPath :: IO ()
renderPath = do
  t <- fakerTile
  let f = concatMap (decPolygon . map fromIntegral) . filterLayerByName "roads" <$> t
  let tbDrawn = map polygonToPoints <$> f
  let path = foldr1 D.atop . map drawTour <$> tbDrawn
  maybe (putStrLn "nothing") render2DVector path


polygonToPoints :: PolygonG -> [Point]
polygonToPoints p = concat [parameters $ pLineTo p, parameters $ pClosePath p]
