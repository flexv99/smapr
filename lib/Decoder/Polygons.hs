{-# LANGUAGE OverloadedStrings #-}

module Decoder.Polygons
  ( decPolygon
  , PolygonG(..)
  , Point(..)
  , GeoAction(..)
  ) where

import Decoder.Helper
import qualified Data.Aeson as A
import Data.List
import Control.Lens

instance A.ToJSON PolygonG where
  toJSON (PolygonG pMoveTo pLineTo pClosePath) =
        A.object ["move_to" A..= pMoveTo, "line_to" A..= pLineTo, "close_path" A..= pClosePath]

  toEncoding (PolygonG pMoveTo pLineTo pClosePath) =
        A.pairs $ "move_to" A..= pMoveTo <> "line_to" A..= pLineTo <> "close_path" A..= pClosePath

decodePolygonCommands :: [Int] -> [[GeoAction]]
decodePolygonCommands r = splitAtMove $ map singleDecoder (splitCommands r)
  where
    singleDecoder (l:ls) = GeoAction
      { _command = decodeCommand l
      , _parameters = tuplify $ map decodeParam ls
      }

absolutePolygonG :: PolygonG -> PolygonG
absolutePolygonG p = set (pClosePath . parameters) [closePath] $ set (pLineTo . parameters) progSumLineTo p
  where
    sumMoveTo = foldl1 sumTuple (view (pMoveTo . parameters) p)
    progSumLineTo = tail $ scanl sumTuple sumMoveTo (view (pLineTo . parameters) p)
    closePath = last $ view (pMoveTo . parameters) p

relativeMoveTo :: [PolygonG] -> [PolygonG]
relativeMoveTo = f []
  where
    f _ []        = []
    f acc (p:ps)  = if not $ null acc
                    then set pMoveTo (newMoveTo p acc) p : f (p : acc) ps
                    else p : f (p : acc) ps
    newMoveTo p c = GeoAction
                      { _command = view (pMoveTo . command) p
                      , _parameters = zipWith sumTuple (view (pMoveTo . parameters) p) [sumMoveToAndLineTo c]
                      }

-- refactor: is extractPoints even needed??
sumMoveToAndLineTo :: [PolygonG] -> Point
sumMoveToAndLineTo polygons = 
    let extractPoints geoAction = if view (command . cmd) geoAction /= ClosePath then view parameters geoAction else []
        allPoints = concatMap (\polygon -> extractPoints (view pMoveTo polygon) ++ extractPoints (view pLineTo polygon)) polygons
    in foldl' sumTuple (0, 0) allPoints

decPolygon :: [Int] -> [PolygonG]
decPolygon = map absolutePolygonG . relativeMoveTo . (map actionToPolygonG . decodePolygonCommands)
   where
    actionToPolygonG :: [GeoAction] -> PolygonG
    actionToPolygonG g = PolygonG { _pMoveTo = head g , _pLineTo = g !! 1, _pClosePath = last g }

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
testShoelace = [(1, 6), (3, 1), (7, 2), (4, 4), (8, 5)]

polygonParams :: PolygonG -> [Point]
polygonParams (PolygonG pMoveTo pLineTo pClosePath) = concat [view parameters pMoveTo, view parameters pLineTo, view parameters pClosePath]

shoelace :: [Point] -> Double
shoelace p = sh' p / 2
  where
    sh' []                       = 0.0
    sh' [x]                      = shoelaceStep x fst
    sh' (x:x':xs)                = shoelaceStep x x' +  sh' (x':xs)
    fst                          = head p
    shoelaceStep (x, y) (x', y') = (x * y') - (y * x')

isInner :: PolygonG -> Bool
isInner = (< 0) . shoelace . polygonParams
