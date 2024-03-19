module Decoder.Geometry
  (Geometry(..)
  , CommandA(..)
  , decodeCommands
  , geometryCommand
  ) where

import Data.Bits
import Control.Monad
import Proto.Vector_tile
import Graphics.Svg
import GHC.Float (int2Double)

type Point = (Double, Double)

data CommandA = MoveTo | LineTo | ClosePath deriving (Show, Eq, Enum, Bounded)

data Command = Command
  { cmd :: CommandA
  , count :: Int
  } deriving (Show, Eq)

data Geometry = Geometry
  { command :: Command
  , parameters :: [Point]
  }
 deriving (Show, Eq)

coordsOrigin :: Point
coordsOrigin = (0.0, 0.0)

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

-- TODO need to sum to get the actual coodinates
decodeCommands :: [Int] -> [Geometry]
decodeCommands r = toAbsoluteCoords coordsOrigin $ map (\c -> singleDecoder c) (splitCommands r)
  where
    singleDecoder (l:ls) = Geometry
      { command = decodeCommand l
      , parameters = tuplify $ map (decodeParam) ls
      }

toAbsoluteCoords :: Point -> [Geometry] -> [Geometry]
toAbsoluteCoords _ []         = []
toAbsoluteCoords point (x:xs) = Geometry
  { command = command x
  , parameters = relativeParams $ sumFirst (parameters x)
  } : toAbsoluteCoords (last (relativeParams (parameters x))) xs
  where
    sumFirst []              = []
    sumFirst (y:ys)          = sumTuple point y : ys
    relativeParams p         = scanl1 sumTuple p
    sumTuple (x, y) (x', y') = (x + x', y + y')
    
geometryCommand :: Geometry -> CommandA
geometryCommand = cmd . command

tuplify :: [a] -> [(a, a)]
tuplify []        = []
tuplify [x]       = error "cannot tuplify single emelent"
tuplify (x:x':xs) = (x, x') : tuplify xs

testLine :: [Int]
testLine = [9, 4, 4, 18, 0, 16, 16, 0]

