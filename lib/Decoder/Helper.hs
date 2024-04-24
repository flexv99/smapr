{-# LANGUAGE OverloadedStrings #-}

module Decoder.Helper where

import Data.Bits
import Control.Monad
import GHC.Float (int2Double)
import GHC.Word
import Data.Foldable
import qualified Data.Sequence as S
import qualified Data.Aeson as A
import Proto.Vector_tile
import Proto.Vector_tile.Tile.GeomType
import Proto.Vector_tile.Tile.Layer
import Proto.Vector_tile.Tile.Feature

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

instance A.ToJSON GeoAction where
  toJSON (GeoAction command parameters) =
        A.object ["command" A..= command, "parameters" A..= parameters]

instance A.ToJSON Command where
  toJSON (Command cmd count) =
        A.object ["cmd" A..= show cmd, "count" A..= count]

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
  if null taken then [] else taken : splitCommands rest
  where
    toBeSplitted = parametersCount (decodeCommand $ head c) + 1

tuplify :: [a] -> [(a, a)]
tuplify []        = []
tuplify [x]       = error "cannot tuplify single emelent"
tuplify (x:x':xs) = (x, x') : tuplify xs

splitAtMove :: [GeoAction] -> [[GeoAction]]
splitAtMove xs = filter (not . null) $ f xs []
    where f [] agg = [agg]
          f (y : ys) agg = if ((MoveTo ==) . cmd . command) y
                           then agg : f ys [y]
                           else f ys (agg ++ [y])

sumTuple :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
sumTuple (x, y) (x', y') = (x + x', y + y')

data PolygonG = PolygonG
  { pMoveTo :: GeoAction
  , pLineTo :: GeoAction
  , pClosePath :: GeoAction
  } deriving (Show, Eq)

data LineG = LineG
  { lMoveTo :: GeoAction
  , lLineTo :: GeoAction
  } deriving (Show, Eq)

data PointG = PointG
  { pMoveT :: GeoAction
  } deriving (Show, Eq)

