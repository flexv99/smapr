module Decoder.Points (PointG (..), decPoint) where

import Decoder.Helper
import Lens.Micro
import Lens.Micro.Extras

decodePointCommands :: [Int] -> [GeoAction]
decodePointCommands = map singleDecoder . splitCommands
  where
    singleDecoder l =
      GeoAction
        { _command = decodeCommand (head l)
        , _parameters = tuplify $ map decodeParam (tail l)
        }

-- absolutePointG :: PointG -> PointG
-- absolutePointG p = set (lLineTo . parameters) progSumLineTo p
--   where
--     sumMoveTo = foldl1 sumTuple $ view (lMoveTo . parameters) p
--     progSumLineTo = tail $ scanl sumTuple sumMoveTo $ view (lLineTo . parameters) p

relativePoint :: [PointG] -> [PointG]
relativePoint = f []
  where
    f _ [] = []
    f acc (p : ps) =
      if not $ null acc
        then set pMoveT (newMoveTo p acc) p : f (p : acc) ps
        else p : f (p : acc) ps
    newMoveTo p c =
      GeoAction
        { _command = view (pMoveT . command) p
        , _parameters = zipWith sumTuple (view (pMoveT . parameters) p) [sumMoveTo c]
        }

sumMoveTo :: [PointG] -> Point
sumMoveTo points = foldl' sumTuple coordsOrigin allPoints
  where
    allPoints = concatMap (\polygon -> view (pMoveT . parameters) polygon) points

decPoint :: [Int] -> [PointG]
decPoint pts = relativePoint [actionToPointG $ decodePointCommands pts]
  where
    actionToPointG :: [GeoAction] -> PointG
    actionToPointG g = PointG{_pMoveT = head g}
