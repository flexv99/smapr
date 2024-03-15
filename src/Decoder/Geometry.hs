module Decoder.Geometry where

import Data.Bits
import Control.Monad
import Proto.Vector_tile
import Graphics.Svg

type Point = (Int, Int)

data CommandA = MoveTo | LineTo | ClosePath deriving Show

data Command = Command
  { cmd :: CommandA
  , count :: Int
  } deriving Show

-- https://protobuf.dev/programming-guides/encoding/#signed-ints
decodeZigzag :: Int -> Int
decodeZigzag n = shiftR n 1 `xor` (-(n .&. 1))

-- command:
-- 3 bits
toCommandA :: Int -> CommandA
toCommandA 1 = MoveTo    -- [001]
toCommandA 2 = LineTo    -- [010]
toCommandA _ = ClosePath -- [111]

decodeCommand :: Int -> Command
decodeCommand c = Command { cmd = cType, count = paramC}
  where
    cType = toCommandA (c .&. 0x7)
    paramC = c `shiftR` 3

-- params:
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

decodeParam :: Int -> Int
decodeParam p = (p `shiftR` 1) `xor` (-(p .&. 1))

splitCommands :: [Int] -> [[Int]]
splitCommands [] = []
splitCommands c = let (taken, rest) = splitAt toBeSplitted c in
  if length taken == 0 then [] else taken : splitCommands rest
  where
    toBeSplitted = (parametersCount $ decodeCommand $ head c) + 1

testLine :: [Int]
testLine = [9, 4, 4, 18, 0, 16, 16, 0]

