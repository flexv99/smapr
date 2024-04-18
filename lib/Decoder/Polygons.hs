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

decodeLineCommands :: [Int] -> [[GeoAction]]
decodeLineCommands r = splitAtMove $ toAbsoluteCoords coordsOrigin $ map singleDecoder (splitCommands r)
  where
    singleDecoder (l:ls) = GeoAction
      { command = decodeCommand l
      , parameters = tuplify $ map decodeParam ls
      }

decodePolygonCommands :: [Int] -> [[GeoAction]]
decodePolygonCommands r = splitAtMove $ toAbsoluteCoords coordsOrigin $ concat $  map (\x -> map (singleDecoder) x) (splitOnSingle $ splitCommands r)
  where
    singleDecoder (l:ls) = GeoAction
  
      { command = decodeCommand l
      , parameters = tuplify $ map decodeParam ls
      }


splitOnSingle :: [[a]] -> [[[a]]]
splitOnSingle [] = []
splitOnSingle ([]:ys) = splitOnSingle ys
splitOnSingle (y:ys)  = let (as, b) = span (\z -> length z > 1) (y:ys)
                        in if null b
                           then [as]
                           else (as <> [head b]) : splitOnSingle (tail b)


decPolygon :: [Int] -> [PolygonG]
decPolygon = map actionToPolygonG . decodePolygonCommands
 where
  actionToPolygonG :: [GeoAction] -> PolygonG
  actionToPolygonG g = PolygonG { pMoveTo = head g , pLineTo = excludeFstLst g, closePath = last g }

testPolygon :: [Int]
testPolygon = [ 9 ,0 ,0 ,26 ,20 ,0 ,0 ,20 ,19 ,0 ,15 ,9 ,22 ,2 ,26 ,18 ,0 ,0 ,18 ,17 ,0 ,15 ,9 ,4 ,13 ,26 ,0 ,8 ,8 ,0 ,0 ,7 ,15 ]
