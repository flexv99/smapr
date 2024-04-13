module Decoder.Polygon where

import Decoder.Geometry

-- criteria inner/outer polygon
-- https://en.wikipedia.org/wiki/Shoelace_formula
-- p1 = (1, 6), p2 = (3, 1), p3 = (7, 2)
-- |1 3|    |3 7|   |7  1|
-- |   | +  |   | + |    | -- matrix multp.
-- |6 1|    |1 2|   |2  6|
-- res is negative: inner polygon


decodeCommands :: [Int] -> [Geometry]
decodeCommands r = toAbsoluteCoords coordsOrigin $ concat $ map pointOfClosePath $ map (\x -> map (singleDecoder) x) (splitOnSingle $ splitCommands r)
  where
    singleDecoder (l:ls) = Geometry
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

pointOfClosePath :: [Geometry] -> [Geometry]
pointOfClosePath geo = map (\ g -> if (cmd (command g)) == ClosePath
                             then Geometry {command = command g, parameters = parameters $ head geo}
                             else g) geo
