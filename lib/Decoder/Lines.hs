module Decoder.Lines 
  ( decLine
  ) where

import Decoder.Geometry

data LineG = LineG
  { lMoveTo :: GeoAction
  , lLineTo :: [GeoAction]
  } deriving (Show, Eq)

decodeLineCommands :: [Int] -> [[GeoAction]]
decodeLineCommands r = splitAtMove $ toAbsoluteCoords coordsOrigin $ map (singleDecoder) (splitCommands r)
  where
    singleDecoder (l:ls) = GeoAction
      { command = decodeCommand l
      , parameters = tuplify $ map (decodeParam) ls
      }

decLine :: [Int] -> [LineG]
decLine = map (actionToLineG) . decodeLineCommands
 where
  actionToLineG :: [GeoAction] -> LineG
  actionToLineG g = LineG { lMoveTo = head g , lLineTo = tail g }

instance MapGeometry LineG where
  decode = decLine
