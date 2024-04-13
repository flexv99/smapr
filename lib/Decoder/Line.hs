module Decoder.Line where

import Decoder.Geometry

decodeLineCommands :: [Int] -> [Geometry]
decodeLineCommands r = toAbsoluteCoords coordsOrigin $ map (singleDecoder) (splitCommands r)
  where
    singleDecoder (l:ls) = Geometry
      { command = decodeCommand l
      , parameters = tuplify $ map (decodeParam) ls
      }
