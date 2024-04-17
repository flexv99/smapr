{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Renderer.Polygons () where

import Diagrams.Prelude
import Diagrams.TwoD.Size
import Diagrams.Backend.SVG
import Diagrams.Trail
import Util
import ApiClient
import Decoder.Geometry
import qualified Decoder.Polygon as P

-- geoToSvgPath :: Geometry -> T.Text
-- geoToSvgPath g = case geometryCommand g of
--   MoveTo    -> T.intercalate " " $ map (uncurry mA) (parameters g)
--   LineTo    -> T.intercalate " " $ map (uncurry lA) (parameters g)
--   ClosePath -> z

-- MoveTo needs pathFromTrailAt from path, which is needed to combine all trails
-- ref: https://hackage.haskell.org/package/diagrams-lib-1.4.6.1/docs/Diagrams-Path.html#v:pathFromTrailAt

-- ClosedPath needs closeLine which needs to know the trail that needs to be closed though
-- ref: https://hackage.haskell.org/package/diagrams-lib-1.4.6.1/docs/Diagrams-Trail.html#v:closeLine

geoToTrail :: Geometry -> Trail' Line V2 Double
geoToTrail (Geometry (Command LineTo _) p) = lineFromOffsets $ map r2 p 
gooToTrail _ = emptyTrail

myCircle :: Diagram B
myCircle = circle 1 `atop` square (sqrt 2)

renderTile :: IO ()
renderTile = do
  let sz = mkSizeSpec2D (Just 512) (Just 512)
  dateString <- dateTimeStr
  path <- testPath dateString
  putStrLn path
  renderSVG path sz myCircle

render2DVector :: Diagram B -> IO ()
render2DVector v = do
  let sz = mkSizeSpec2D (Just 512) (Just 512)
  dateStr <- dateTimeStr
  path <- testPath dateStr
  putStrLn path
  renderSVG path sz $ v # showOrigin

splitAtMove :: [Geometry] -> [[Geometry]]
splitAtMove xs = filter (not . null) $ f xs []
    where f [] agg = [agg]
          f (y : ys) agg = if ((MoveTo ==) . cmd . command) y
                           then agg : (f ys [y])
                           else f ys (agg ++ [y])

testSplit = do
  t <- fakerTile
  let water = concatMap (P.decodeCommands . map fromIntegral) . filterLayerByName "water" <$> t
  return $ splitAtMove <$> water
