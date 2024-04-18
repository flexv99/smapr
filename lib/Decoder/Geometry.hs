{-# LANGUAGE OverloadedStrings #-}

module Decoder.Geometry
  ( GeoAction(..)
  , Command(..)
  , CommandA(..)
  , decodeCommands
  , decodeCommand
  , toAbsoluteCoords
  , coordsOrigin
  , splitCommands
  , decodeParam
  , tuplify
  , geometryCommand
  ) where

import Data.Bits
import Control.Monad
import GHC.Float (int2Double)
import GHC.Word
import Proto.Vector_tile
import Data.Aeson
import Graphics.Svg

type Point = (Double, Double)

data CommandA = MoveTo | LineTo | ClosePath deriving (Show, Eq, Enum, Bounded)

data Command = Command
  { cmd :: CommandA
  , count :: Int
  } deriving (Show, Eq)

data GeoAction = GeoAction
  { command :: Command
  , parameters :: [Point]
  } deriving (Show, Eq)  

instance ToJSON GeoAction where
  toJSON (GeoAction command parameters) =
        object ["command" .= command, "parameters" .= parameters]

instance ToJSON Command where
  toJSON (Command cmd count) =
        object ["cmd" .= show cmd, "count" .= count]

coordsOrigin :: Point
coordsOrigin = (0.0, 0.0)

geometryCommand :: GeoAction -> CommandA
geometryCommand = cmd . command

-- command:
-- 3 bits
toCommandA :: Int -> CommandA
toCommandA 1 = MoveTo    -- [001]
toCommandA 2 = LineTo    -- [010]
toCommandA _ = ClosePath -- [111]

decodeCommand :: Int -> Command
decodeCommand c = Command
  { cmd = cType
  , count = paramC
  }
  where
    cType = toCommandA (c .&. 0x7)
    paramC = c `shiftR` 3

-- parameters:
-- number of params is determined by command cound multiplied by parameter count
-- moveTo has a parameter count of 2
-- lineTo has a parameter count of 2
-- closePath has a paramete count of 0

parametersCount :: Command -> Int
parametersCount = ap ((*) . forAction . cmd) count -- ap promotes function application
  where
    forAction :: CommandA -> Int
    forAction MoveTo = 2
    forAction LineTo = 2
    forAction _      = 0

-- https://protobuf.dev/programming-guides/encoding/#signed-ints
decodeParam :: Int -> Double
decodeParam p = int2Double $ (p `shiftR` 1) `xor` (-(p .&. 1))

splitCommands :: [Int] -> [[Int]]
splitCommands [] = []
splitCommands c  = let (taken, rest) = splitAt toBeSplitted c in
  if length taken == 0 then [] else taken : splitCommands rest
  where
    toBeSplitted = (parametersCount $ decodeCommand $ head c) + 1

tuplify :: [a] -> [(a, a)]
tuplify []        = []
tuplify [x]       = error "cannot tuplify single emelent"
tuplify (x:x':xs) = (x, x') : tuplify xs

testLine :: [Int]
testLine = [9, 4, 4, 18, 0, 16, 16, 0]

testPolygon :: [Int]
testPolygon = [9, 0, 0, 26, 20, 0, 0, 20, 19, 0, 15, 9, 22, 2, 26, 18, 0, 0, 18, 17, 0, 15, 9, 4, 13, 26, 0, 8, 8, 0, 0, 7, 15]

decodeCommands :: [Int] -> [GeoAction]
decodeCommands = undefined

toAbsoluteCoords :: Point -> [GeoAction] -> [GeoAction]
toAbsoluteCoords _ []         = []
toAbsoluteCoords point (x:xs) =
  let geo = GeoAction

        { command = command x
        , parameters = relativeParams $ sumFirst (parameters x)
        } in geo : toAbsoluteCoords (last (parameters geo)) xs
  where
    sumFirst []              = []
    sumFirst (y:ys)          = sumTuple point y : ys
    relativeParams           = scanl1 sumTuple
    sumTuple (x, y) (x', y') = (x + x', y + y')
