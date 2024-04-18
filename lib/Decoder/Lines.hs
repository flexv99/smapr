module Decoder.Lines where

import Decoder.Geometry

decodeLineCommands :: [Int] -> [GeoAction]
decodeLineCommands r = toAbsoluteCoords coordsOrigin $ map (singleDecoder) (splitCommands r)
  where
    singleDecoder (l:ls) = GeoAction
      { command = decodeCommand l
      , parameters = tuplify $ map (decodeParam) ls
      }
