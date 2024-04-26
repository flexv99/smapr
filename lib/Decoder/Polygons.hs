{-# LANGUAGE OverloadedStrings #-}

module Decoder.Polygons
  ( decPolygon
  , PolygonG(..)
  , Point(..)
  , GeoAction(..)
  ) where

import Decoder.Helper
import Data.Aeson
import Data.List

instance ToJSON PolygonG where
  toJSON (PolygonG pMoveTo pLineTo pClosePath) =
        object ["move_to" .= pMoveTo, "line_to" .= pLineTo, "close_path" .= pClosePath]

  toEncoding (PolygonG pMoveTo pLineTo pClosePath) =
        pairs $ "move_to" .= pMoveTo <> "line_to" .= pLineTo <> "close_path" .= pClosePath

decodePolygonCommands :: [Int] -> [[GeoAction]]
decodePolygonCommands r = splitAtMove $ map singleDecoder (splitCommands r)
  where
    singleDecoder (l:ls) = GeoAction
      { command = decodeCommand l
      , parameters = tuplify $ map decodeParam ls
      }

absolutePolygonG :: PolygonG -> PolygonG
absolutePolygonG p = PolygonG { pMoveTo = pMoveTo p
                              , pLineTo = GeoAction { command = command $ pLineTo p, parameters = progSumLineTo }
                              , pClosePath = GeoAction { command = command $ pClosePath p, parameters = [closePath] }
                              }
  where
    sumMoveTo = foldl1 sumTuple (parameters $ pMoveTo p)
    progSumLineTo = tail $ scanl sumTuple sumMoveTo (parameters $ pLineTo p)
    closePath = last $ parameters $ pMoveTo p

relativeMoveTo :: [PolygonG] -> [PolygonG]
relativeMoveTo = f []
  where
    f _ []        = []
    f acc (p:ps)  = if not $ null acc
                    then PolygonG { pMoveTo = newMoveTo p acc, pLineTo = pLineTo p, pClosePath = pClosePath p } : f (p : acc) ps
                    else p : f (p : acc) ps
    newMoveTo p c = GeoAction { command = command $ pMoveTo p , parameters = zipWith sumTuple (parameters $ pMoveTo p) [sumMoveToAndLineTo c]}

sumMoveToAndLineTo :: [PolygonG] -> Point
sumMoveToAndLineTo polygons =
    let extractPoints geoAction = if cmd (command geoAction) == MoveTo || cmd (command geoAction) == LineTo then parameters geoAction else []
        allPoints = concatMap (\polygon -> extractPoints (pMoveTo polygon) ++ extractPoints (pLineTo polygon)) polygons
    in foldl' sumTuple (0, 0) allPoints

decPolygon :: [Int] -> [PolygonG]
decPolygon = map absolutePolygonG . relativeMoveTo . (map actionToPolygonG . decodePolygonCommands)
   where
    actionToPolygonG :: [GeoAction] -> PolygonG
    actionToPolygonG g = PolygonG { pMoveTo = head g , pLineTo = g !! 1, pClosePath = last g }

-- criteria inner/outer polygon
-- https://en.wikipedia.org/wiki/Shoelace_formula
-- p1 = (1, 6), p2 = (3, 1), p3 = (7, 2)
-- |1 3|    |3 7|   |7  1|
-- |   | +  |   | + |    | -- matrix multp.
-- |6 1|    |1 2|   |2  6|
-- res is negative: inner polygon

testPolygon :: [Int]
testPolygon = [9,0,0,26,20,0,0,20,19,0,15,9,22,2,26,18,0,0,18,17,0,15,9,4,13,26,0,8,8,0,0,7,15]

testShoelace :: [Point]
testShoelace = [(1, 6), (3, 1), (7, 2), (4, 4), (8, 6)]

polygonParams :: PolygonG -> [Point]
polygonParams (PolygonG pMoveTo pLineTo pClosePath) = concat [parameters pMoveTo, parameters pLineTo, parameters pClosePath]

shoelace :: [Point] -> Double
shoelace p = sh' p / 2
  where
    sh' []          = 0.0
    sh' [x, y]      = shoelaceStep x y
    sh' (x:x':xs)   = shoelaceStep x x' +  sh' (x':xs)
    shoelaceStep (x, y) (x', y') = x * y' - (y * x')

isInner :: PolygonG -> bool
isInner = undefined
