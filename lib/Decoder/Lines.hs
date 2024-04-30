{-# LANGUAGE OverloadedStrings #-}

module Decoder.Lines (
  decLine
  , LineG(..)
  , Point(..)
  , GeoAction(..)
  ) where

import Decoder.Helper
import Data.Aeson
import Data.List
import Control.Lens

-- instance ToJSON LineG where
--   toJSON (LineG pMoveTo pLineTo) =
--         object ["move_to" .= pMoveTo, "line_to" .= pLineTo]

--   toEncoding (LineG pMoveTo pLineTo) =
--         pairs $ "move_to" .= pMoveTo <> "line_to" .= pLineTo

decodeLineCommands :: [Int] -> [[GeoAction]]
decodeLineCommands = splitAtMove . map singleDecoder . splitCommands
  where
    singleDecoder (l:ls) = GeoAction
      { _command = decodeCommand l
      , _parameters = tuplify $ map decodeParam ls
      }

absoluteLineG :: LineG -> LineG
absoluteLineG p = set (lLineTo . parameters) progSumLineTo p
  where
    sumMoveTo     = foldl1 sumTuple $ view (lMoveTo . parameters) p
    progSumLineTo = tail $ scanl sumTuple sumMoveTo $ view (lLineTo . parameters) p
    closePath     = last $ view (lMoveTo . parameters) p

relativeMoveTo :: [LineG] -> [LineG]
relativeMoveTo = f []
  where
    f _ []        = []
    f acc (p:ps)  = if not $ null acc
                    then set lMoveTo (newMoveTo p acc) p : f (p : acc) ps
                    else p : f (p : acc) ps
    newMoveTo p c = GeoAction { _command = view (lMoveTo . command) p
                              , _parameters = zipWith sumTuple (view (lMoveTo . parameters) p) [sumMoveToAndLineTo c]}

sumMoveToAndLineTo :: [LineG] -> Point
sumMoveToAndLineTo polygons = foldl' sumTuple (0, 0) allPoints
    where      
      allPoints = concatMap (\polygon -> view (lMoveTo . parameters) polygon ++ view (lLineTo . parameters) polygon) polygons

decLine :: [Int] -> [LineG]
decLine = map absoluteLineG . relativeMoveTo . (map actionToLineG . decodeLineCommands)
   where
    actionToLineG :: [GeoAction] -> LineG
    actionToLineG g = LineG { _lMoveTo = head g , _lLineTo = last g }
