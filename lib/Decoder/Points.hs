module Decoder.Points (
  PointG (..),
  decPoint,
) where

import Data.List (foldl')
import Decoder.Helper
import Lens.Micro
import Lens.Micro.Extras

relativeMoveTo :: [PointG] -> [PointG]
relativeMoveTo = f []
  where
    f _ [] = []
    f acc (p : ps) =
      if not $ null acc
        then (newMoveTo p acc) : f (p : acc) ps
        else p : f (p : acc) ps
    newMoveTo p c =
      GeoAction
        { _command = p ^. command
        , _parameters = zipWith sumTuple (p ^. parameters) [sumMoveToAndLineTo c]
        }

sumMoveToAndLineTo :: [PointG] -> Point
sumMoveToAndLineTo polygons = foldl' sumTuple coordsOrigin allPoints
  where
    allPoints = concatMap (\polygon -> (polygon ^. parameters)) polygons

decPoint :: [Int] -> [PointG]
decPoint = relativeMoveTo . map singleDecoder . splitCommands
  where
    singleDecoder (l : ls) =
      GeoAction
        { _command = decodeCommand l
        , _parameters = progSumLineTo $ tuplify $ map decodeParam ls
        }
    singleDecoder _ = error "should newer happen"
    progSumLineTo = scanl1 sumTuple
