{-# LANGUAGE OverloadedStrings #-}

module Decoder.Lines (
  decLine,
  LineG (..),
  Point,
  GeoAction (..),
)
where

import Decoder.Helper
import Lens.Micro
import Lens.Micro.Extras

decodeLineCommands :: [Int] -> [[GeoAction]]
decodeLineCommands = splitAtMove . map singleDecoder . splitCommands
  where
    singleDecoder (l : ls) =
      GeoAction
        { _command = decodeCommand l
        , _parameters = tuplify $ map decodeParam ls
        }
    singleDecoder _ = error "should newer happen"

absoluteLineG :: LineG -> LineG
absoluteLineG p = set (lLineTo . parameters) progSumLineTo p
  where
    sumMoveTo = foldl1 sumTuple $ view (lMoveTo . parameters) p
    progSumLineTo = drop 1 $ scanl sumTuple sumMoveTo $ view (lLineTo . parameters) p

relativeMoveTo :: [LineG] -> [LineG]
relativeMoveTo = f []
  where
    f _ [] = []
    f acc (p : ps) =
      if not $ null acc
        then set lMoveTo (newMoveTo p acc) p : f (p : acc) ps
        else p : f (p : acc) ps
    newMoveTo p c =
      GeoAction
        { _command = view (lMoveTo . command) p
        , _parameters = zipWith sumTuple (view (lMoveTo . parameters) p) [sumMoveToAndLineTo c]
        }

sumMoveToAndLineTo :: [LineG] -> Point
sumMoveToAndLineTo polygons = foldl' sumTuple coordsOrigin allPoints
  where
    allPoints = concatMap (\polygon -> view (lMoveTo . parameters) polygon ++ view (lLineTo . parameters) polygon) polygons

decLine :: [Int] -> [LineG]
decLine = map absoluteLineG . relativeMoveTo . (map actionToLineG . decodeLineCommands)
  where
    actionToLineG :: [GeoAction] -> LineG
    actionToLineG (g : []) = LineG{_lMoveTo = g, _lLineTo = g}
    actionToLineG (g : gs) = LineG{_lMoveTo = g, _lLineTo = last gs}
    actionToLineG _ = error "should newer happen"
