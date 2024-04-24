{-# LANGUAGE OverloadedStrings #-}

module Renderer.Lines
  ( lineToPoints
  ) where

import Control.Monad
import qualified Data.Text as T
import qualified Diagrams.Prelude as D
import qualified Diagrams.TwoD.Size as D
import qualified Diagrams.Backend.SVG as D
import qualified Diagrams.Trail as D
import Graphics.Svg
import Util
import ApiClient
import Decoder.Geometry
import Decoder.Lines
import Proto.Vector_tile.Tile (Tile(..))

-- svg :: Element -> Element
-- svg content =
--   doctype
--   <> with (svg11_ content) [Version_ <<- "1.1", Width_ <<- "100%", Height_ <<- "100%"]

-- renderCommands :: [GeoAction] -> Element
-- renderCommands g =
--   path_ [
--   Fill_           <<- "None"
--   , Stroke_       <<- "#ffffff"
--   , Stroke_width_ <<- "4"
--   , D_            <<- T.intercalate " " ( map geoToSvgPath g )
--         ]

-- renderBuildingCommands :: [GeoAction] -> Element
-- renderBuildingCommands g =
--   path_ [
--   Fill_           <<- "#f54242"
--   , Stroke_       <<- "#ffffff"
--   , Stroke_width_ <<- "4"
--   , D_            <<- T.intercalate " " ( map geoToSvgPath g )
--         ]

-- geoToSvgPath :: GeoAction -> T.Text
-- geoToSvgPath g = case geometryCommand g of
--   MoveTo    -> T.intercalate " " $ map (uncurry mA) (parameters g)
--   LineTo    -> T.intercalate " " $ map (uncurry lA) (parameters g)
--   ClosePath -> z

-- testSvg :: IO (Maybe Element)
-- testSvg = do
--   tile <- fakerTile
--   let features = concatMap (P.decodeCommands . map fromIntegral) . tileFeatures <$> tile
--   return (svg . renderCommands <$> features)


-- polyTestSvg :: IO (Maybe Element)
-- polyTestSvg = do
--   tile <- getNextzenTile testCoord
--   let features = concatMap (P.decodeCommands . map fromIntegral) . filterLayerByName "buildings" <$> tile
--   return (svg . renderCommands <$> features)

-- saveTestSvg :: IO ()
-- saveTestSvg = testSvg >>= maybe (putStrLn "got nothing") writeSvg
-------- Diagrams

render2DVector :: D.Diagram D.B -> IO ()
render2DVector v = do
  let sz = D.mkSizeSpec2D (Just 512) (Just 512)
  dateStr <- dateTimeStr
  path <- testPath dateStr
  putStrLn path
  D.renderSVG path sz $ v D.# D.showOrigin

geometryPointToDPoint :: Point -> D.P2 Double
geometryPointToDPoint (x, y) = x D.^& y 

lineToPoints :: LineG -> [D.P2 Double]
lineToPoints (LineG lMoveTo lLineTo) = toDPoint $ parameters lMoveTo ++ parameters lLineTo
  where
    toDPoint = map geometryPointToDPoint

-- drawTour :: [Point] -> D.Diagram D.B
-- drawTour tour = tourPoints <> D.strokeP tourPath
--   where
--     tourPath   = D.fromVertices . map geometryPointToDPoint $ tour
--     tourPoints = D.atPoints (concat . D.pathVertices $ tourPath) (repeat dot)
--     dot = D.circle 0.05 D.# D.fc D.green

-- renderPath :: IO ()
-- renderPath = do
--   t <- fakerTile
--   let f = concatMap (decode . map fromIntegral) . filterLayerByName "roads" <$> t
--   let tbDrawn = map (parameters . lLineTo) <$> f
--   let path = foldr1 D.atop . map drawTour <$> tbDrawn
--   maybe (putStrLn "nothing") render2DVector path
