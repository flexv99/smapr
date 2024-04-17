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

geoToPath :: [Geometry] -> Path V2 Double
geoToPath g = start (head g) (mconcat (map (wrapTrail . actionForCommand) g))
  where
    actionForCommand :: Geometry -> Trail' Line V2 Double
    actionForCommand (Geometry (Command LineTo _) p)    = lineFromOffsets $ map r2 p
  -- actionForCommand (Geometry (Command ClosePath _) p) = closeLine p
    actionForCommand _                                  = emptyLine
    start :: Geometry -> Trail V2 Double -> Path V2 Double
    start m p = pathFromTrailAt p $ P $ r2 $ head $ parameters m

tmp :: Renderable (Path V2 Double) b =>
       [Geometry] -> QDiagram b V2 Double Any
tmp g = strokeLine (mconcat (map (actionForCommand) g))
  where
    actionForCommand :: Geometry -> Trail' Line V2 Double
    actionForCommand (Geometry (Command LineTo _) p)    = lineFromOffsets $ map r2 p
  -- actionForCommand (Geometry (Command ClosePath _) p) = closeLine p
    actionForCommand _                                  = emptyLine

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

testP = tmp [Geometry {command = Command {cmd = MoveTo, count = 1}, parameters = [(8192.0,5278.0)]},Geometry {command = Command {cmd = LineTo, count = 20}, parameters = [(7878.0,5187.0),(7040.0,5217.0),(6544.0,5145.0),(6358.0,5197.0),(5822.0,5198.0),(5672.0,5128.0),(5558.0,5148.0),(5458.0,5119.0),(5300.0,5133.0),(5142.0,5054.0),(5026.0,5067.0),(4888.0,5025.0),(4802.0,5040.0),(4493.0,4957.0),(4310.0,4848.0),(4167.0,4703.0),(3951.0,4634.0),(3714.0,4448.0),(3494.0,4342.0),(3207.0,4320.0)]}]
