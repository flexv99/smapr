{-# LANGUAGE FlexibleContexts #-}

module Decoder.Geometry
  ( Geometry(..)
  , CommandA(..)
  , decodeCommands
  , geometryCommand
  ) where

import GHC.Word
import GHC.ST (runST, ST)
import Data.Bits
import Data.Array.ST (newArray, readArray, MArray, STUArray)
import Data.Array.Base (castSTUArray)
import Control.Monad
import Proto.Vector_tile
import Graphics.Svg

type Point = (Double, Double)

data CommandA = MoveTo | LineTo | ClosePath deriving (Show, Eq, Enum, Bounded)

data Command = Command
  { cmd :: CommandA
  , count :: Word32
  } deriving (Show, Eq)

data Geometry = Geometry
  { command :: Command
  , parameters :: [Point]
  }
 deriving (Show, Eq)

coordsOrigin :: Point
coordsOrigin = (0.0, 0.0)

geometryCommand :: Geometry -> CommandA
geometryCommand = cmd . command

-- command:
-- 3 bits
toCommandA :: Word32 -> CommandA
toCommandA 1 = MoveTo    -- [001]
toCommandA 2 = LineTo    -- [010]
toCommandA _ = ClosePath -- [111]

decodeCommand :: Word32 -> Command
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

parametersCount :: Command -> Word32
parametersCount = ap ((*) . forAction . cmd) count -- ap promotes function application
  where
    forAction :: CommandA -> Word32
    forAction MoveTo = 2
    forAction LineTo = 2
    forAction _      = 0

-- https://protobuf.dev/programming-guides/encoding/#signed-ints
decodeParam :: Word32 -> Double
decodeParam p = wordToDouble $ (p `shiftR` 1) `xor` (-(p .&. 1))

splitCommands :: [Word32] -> [[Word32]]
splitCommands [] = []
splitCommands c  = let (taken, rest) = splitAt toBeSplitted c in
  if length taken == 0 then [] else taken : splitCommands rest
  where
    toBeSplitted = fromIntegral $ parametersCount $ decodeCommand $ head c + 1

-- TODO need to sum to get the actual coodinates
decodeCommands :: [Word32] -> [Geometry]
decodeCommands r = toAbsoluteCoords coordsOrigin $ map (\c -> singleDecoder c) (splitCommands r)
  where
    singleDecoder :: [Word32] -> Geometry
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

tuplify :: [a] -> [(a, a)]
tuplify []        = []
tuplify [x]       = error "cannot tuplify single emelent"
tuplify (x:x':xs) = (x, x') : tuplify xs

-- https://gitlab.haskell.org/ghc/ghc/-/issues/2209
wordToDouble :: Word32 -> Double
wordToDouble x = runST (cast x)

{-# INLINE cast #-}
cast :: (MArray (STUArray s) a (ST s),
         MArray (STUArray s) b (ST s)) => a -> ST s b
cast x = newArray (0 :: Int, 0) x >>= castSTUArray >>= flip readArray 0

testLine :: [Word32]
testLine = [9, 4, 4, 18, 0, 16, 16, 0]

