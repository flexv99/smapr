module Decoder.Polygons
( decPolygon
) where

import Decoder.Geometry

data PolygonG = PolygonG
  { pMoveTo :: GeoAction
  , pLineTo :: GeoAction
  , pClosePath :: GeoAction
  } deriving (Show, Eq)

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

decodePolygonCommands :: [Int] -> [[GeoAction]]
decodePolygonCommands r = splitAtMove $ map singleDecoder (splitCommands r)
  where
    singleDecoder (l:ls) = GeoAction
      { command = decodeCommand l
      , parameters = tuplify $ map decodeParam ls
      }

decPolygon :: [Int] -> [PolygonG]
decPolygon = map actionToPolygonG . decodePolygonCommands
 where
  actionToPolygonG :: [GeoAction] -> PolygonG
  actionToPolygonG g = absolutePolygonG $ PolygonG { pMoveTo = head g , pLineTo = g !! 1, pClosePath = last g }

absolutePolygonG :: PolygonG -> PolygonG
absolutePolygonG p = PolygonG { pMoveTo = pMoveTo p
                              , pLineTo = GeoAction { command = command $ pLineTo p, parameters = progSumLineTo }
                              , pClosePath = GeoAction { command = command $ pClosePath p, parameters = [closePath] }
                              }
  where
    sumTuple (x, y) (x', y') = (x + x', y + y')
    sumMoveTo = foldl1 sumTuple (parameters $ pMoveTo p)
    progSumLineTo = tail $ scanl sumTuple sumMoveTo (parameters $ pLineTo p)
    closePath = last $ parameters $ pMoveTo p

testPolygon :: [Int]
testPolygon = [9, 0, 0, 26, 20, 0, 0, 20, 19, 0, 15, 9, 22, 2, 26, 18, 0, 0, 18, 17, 0, 15, 9, 4, 13, 26, 0, 8, 8, 0, 0, 7, 15]

instance MapGeometry PolygonG where
  decode = decPolygon
