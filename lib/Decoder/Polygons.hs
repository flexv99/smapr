{-# LANGUAGE OverloadedStrings #-}

module Decoder.Polygons
( decPolygon
) where

import Decoder.Geometry
import Data.Aeson

data PolygonG = PolygonG
  { pMoveTo :: GeoAction
  , pLineTo :: GeoAction
  , pClosePath :: GeoAction
  } deriving (Show, Eq)


instance ToJSON PolygonG where
  toJSON (PolygonG pMoveTo pLineTo pClosePath) =
        object ["move_to" .= pMoveTo, "line_to" .= pLineTo, "close_path" .= pClosePath]

  toEncoding (PolygonG pMoveTo pLineTo pClosePath) =
        pairs $ "move_to" .= pMoveTo <> "line_to" .= pLineTo <> "close_path" .= pClosePath

-- criteria inner/outer polygon
-- https://en.wikipedia.org/wiki/Shoelace_formula
-- p1 = (1, 6), p2 = (3, 1), p3 = (7, 2)
-- |1 3|    |3 7|   |7  1|
-- |   | +  |   | + |    | -- matrix multp.
-- |6 1|    |1 2|   |2  6|
-- res is negative: inner polygon
-- TODO new move to from a polygon is relative to the last line to 
excludeFstLst :: [a] -> [a]
excludeFstLst []  = []
excludeFstLst [x] = []
excludeFstLst xs  = tail (init xs)

sumTuple :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
sumTuple (x, y) (x', y') = (x + x', y + y')

sumTuple3 :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b) -> (a, b)
sumTuple3 (x, y) (x', y') (x'', y'')= (x + x' + x'', y + y' + y'')

decodePolygonCommands :: [Int] -> [[GeoAction]]
decodePolygonCommands r = splitAtMove $ map singleDecoder (splitCommands r)
  where
    singleDecoder (l:ls) = GeoAction
      { command = decodeCommand l
      , parameters = tuplify $ map decodeParam ls
      }

decPolygon :: [Int] -> [PolygonG]
decPolygon = map absolutePolygonG . relativeMoveTo . (map actionToPolygonG . decodePolygonCommands)
 where
  actionToPolygonG :: [GeoAction] -> PolygonG
  actionToPolygonG g = PolygonG { pMoveTo = head g , pLineTo = g !! 1, pClosePath = last g }

absolutePolygonG :: PolygonG -> PolygonG
absolutePolygonG p = PolygonG { pMoveTo = pMoveTo p
                              , pLineTo = GeoAction { command = command $ pLineTo p, parameters = progSumLineTo }
                              , pClosePath = GeoAction { command = command $ pClosePath p, parameters = [closePath] }
                              }
  where
    sumMoveTo = foldl1 sumTuple (parameters $ pMoveTo p)
    progSumLineTo = tail $ scanl sumTuple sumMoveTo (parameters $ pLineTo p)
    closePath = last $ parameters $ pMoveTo p

testPolygon :: [Int]
testPolygon = [9, 0, 0, 26, 20, 0, 0, 20, 19, 0, 15, 9, 22, 2, 26, 18, 0, 0, 18, 17, 0, 15, 9, 4, 13, 26, 0, 8, 8, 0, 0, 7, 15]

instance MapGeometry PolygonG where
  decode = decPolygon

relativeMoveTo :: [PolygonG] -> [PolygonG]
relativeMoveTo = f []
  where
    f _ []        = []
    f acc (p:ps)  = if not $ null acc
                    then PolygonG { pMoveTo = newMoveTo p acc, pLineTo = pLineTo p, pClosePath = pClosePath p} : f (p : acc) ps
                    else p : f (p : acc) ps
    prev          = last
    newMoveTo p c = GeoAction { command = command $ pMoveTo p , parameters = zipWith3 sumTuple3 (parameters $ pMoveTo p) [sumLineTo c] [sumMoveTo c] }
    sumLineTo c   = foldl1 sumTuple (parameters $ pLineTo $ prev c)
    sumMoveTo c   = foldl1 sumTuple (parameters $ pMoveTo $ prev c)
