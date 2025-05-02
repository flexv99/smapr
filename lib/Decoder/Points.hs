module Decoder.Points (
  PointG (..),
  decPoint,
) where

import Data.List (foldl')
import Decoder.Helper
import Lens.Micro
import Lens.Micro.Extras

decPoint :: [Int] -> [PointG]
decPoint = map singleDecoder . splitCommands
  where
    singleDecoder (l : ls) =
      GeoAction
        { _command = decodeCommand l
        , _parameters = tuplify $ map decodeParam ls
        }
    singleDecoder _ = error "should newer happen"
