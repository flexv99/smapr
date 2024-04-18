module Decoder.Polygons
( decPolygon
) where

import Decoder.Geometry

-- criteria inner/outer polygon
-- https://en.wikipedia.org/wiki/Shoelace_formula
-- p1 = (1, 6), p2 = (3, 1), p3 = (7, 2)
-- |1 3|    |3 7|   |7  1|
-- |   | +  |   | + |    | -- matrix multp.
-- |6 1|    |1 2|   |2  6|
-- res is negative: inner polygon
excludeFstLst :: [a] -> [a]
excludeFstLst []  = []
excludeFstLst [x] = []
excludeFstLst xs  = tail (init xs)

decodePolygonCommands :: [Int] -> [[GeoAction]]
decodePolygonCommands r = map (test coordsOrigin) $ splitAtMove $ map singleDecoder (splitCommands r)
  where
    singleDecoder (l:ls) = GeoAction
      { command = decodeCommand l
      , parameters = tuplify $ map decodeParam ls
      }

decPolygon :: [Int] -> [PolygonG]
decPolygon = map actionToPolygonG . decodePolygonCommands
 where
  actionToPolygonG :: [GeoAction] -> PolygonG
  actionToPolygonG g = PolygonG { pMoveTo = head g , pLineTo = excludeFstLst g, closePath = last g }

testPolygon :: [Int]
testPolygon = [ 9 ,0 ,0 ,26 ,20 ,0 ,0 ,20 ,19 ,0 ,15 ,9 ,22 ,2 ,26 ,18 ,0 ,0 ,18 ,17 ,0 ,15 ,9 ,4 ,13 ,26 ,0 ,8 ,8 ,0 ,0 ,7 ,15 ]


test :: Point -> [GeoAction] -> [GeoAction]
test point actions = toAbsoluteCoords' point [] actions

toAbsoluteCoords' :: Point -> [GeoAction] -> [GeoAction] -> [GeoAction]
toAbsoluteCoords' _ acc [] = acc
toAbsoluteCoords' point acc (x:xs) =
  let geoAction = GeoAction
        { command = command x
        , parameters = relativeParams $ sumFirst (parameters x)
        }
  in geoAction : toAbsoluteCoords' (last (parameters geoAction)) (geoAction : acc) xs
  where
    sumFirst []              = parameters $ last acc
    sumFirst (y:ys)          = sumTuple point y : ys
    relativeParams           = scanl1 sumTuple
    sumTuple (x, y) (x', y') = (x + x', y + y')
