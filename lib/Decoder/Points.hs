module Decoder.Points where

import Decoder.Helper


decodePointCommands :: [Int] -> [GeoAction]
decodePointCommands = map singleDecoder . splitCommands
  where
    singleDecoder l = GeoAction { _command = decodeCommand (head l)
                                , _parameters = tuplify $ map decodeParam (tail l)
                                }

decPoint :: [Int] -> PointG
decPoint pts =  actionToPointG $ decodePointCommands pts
  where
    actionToPointG :: [GeoAction] -> PointG
    actionToPointG g = PointG{_pMoveT = head g}
