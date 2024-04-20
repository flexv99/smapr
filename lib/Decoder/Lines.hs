{-# LANGUAGE OverloadedStrings #-}

module Decoder.Lines
  ( decLine
  ) where

import Decoder.Geometry
import Data.Aeson

data LineG = LineG
  { lMoveTo :: GeoAction
  , lLineTo :: [GeoAction]
  } deriving (Show, Eq)


instance ToJSON LineG where
  toJSON (LineG pMoveTo pLineTo) =
        object ["move_to" .= pMoveTo, "line_to" .= pLineTo]

  toEncoding (LineG pMoveTo pLineTo) =
        pairs $ "move_to" .= pMoveTo <> "line_to" .= pLineTo


decodeLineCommands :: [Int] -> [[GeoAction]]
decodeLineCommands = splitAtMove . toAbsoluteCoords coordsOrigin . map singleDecoder . splitCommands
  where
    singleDecoder (l:ls) = GeoAction
      { command = decodeCommand l
      , parameters = tuplify $ map decodeParam ls
      }

decLine :: [Int] -> [LineG]
decLine = map actionToLineG . decodeLineCommands
 where
  actionToLineG :: [GeoAction] -> LineG
  actionToLineG g = LineG { lMoveTo = head g , lLineTo = tail g }

instance MapGeometry LineG where
  decode = decLine
