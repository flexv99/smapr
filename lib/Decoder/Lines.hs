{-# LANGUAGE OverloadedStrings #-}

module Decoder.Lines () where

import Decoder.Geometry
import Data.Aeson
import Data.List

instance ToJSON LineG where
  toJSON (LineG pMoveTo pLineTo) =
        object ["move_to" .= pMoveTo, "line_to" .= pLineTo]

  toEncoding (LineG pMoveTo pLineTo) =
        pairs $ "move_to" .= pMoveTo <> "line_to" .= pLineTo

decodeLineCommands :: [Int] -> [[GeoAction]]
decodeLineCommands = splitAtMove . map singleDecoder . splitCommands
  where
    singleDecoder (l:ls) = GeoAction
      { command = decodeCommand l
      , parameters = tuplify $ map decodeParam ls
      }

absoluteLineG :: LineG -> LineG
absoluteLineG p = LineG { lMoveTo = lMoveTo p
                        , lLineTo = GeoAction { command = command $ lLineTo p, parameters = progSumLineTo }
                        }
  where
    sumMoveTo     = foldl1 sumTuple (parameters $ lMoveTo p)
    progSumLineTo = tail $ scanl sumTuple sumMoveTo (parameters $ lLineTo p)
    closePath     = last $ parameters $ lMoveTo p


relativeMoveTo :: [LineG] -> [LineG]
relativeMoveTo = f []
  where
    f _ []        = []
    f acc (p:ps)  = if not $ null acc
                    then LineG { lMoveTo = newMoveTo p acc, lLineTo = lLineTo p } : f (p : acc) ps
                    else p : f (p : acc) ps
    newMoveTo p c = GeoAction { command = command $ lMoveTo p , parameters = zipWith sumTuple (parameters $ lMoveTo p) [sumMoveToAndLineTo c]}

sumMoveToAndLineTo :: [LineG] -> Point
sumMoveToAndLineTo polygons =
    let extractPoints geoAction = if cmd (command geoAction) == MoveTo || cmd (command geoAction) == LineTo then parameters geoAction else []
        allPoints = concatMap (\polygon -> extractPoints (lMoveTo polygon) ++ extractPoints (lLineTo polygon)) polygons
    in foldl' sumTuple (0, 0) allPoints

instance MapGeometry LineG where
  decode = map absoluteLineG . relativeMoveTo . (map actionToLineG . decodeLineCommands)
   where
    actionToLineG :: [GeoAction] -> LineG
    actionToLineG g = LineG { lMoveTo = head g , lLineTo = last g }
