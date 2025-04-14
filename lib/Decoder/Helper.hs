{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Decoder.Helper where

import Control.Monad
import Data.Bits
import qualified Diagrams.Prelude as D
import GHC.Float
import Lens.Micro.TH

type Point = D.P2 Double

data CommandA = MoveTo | LineTo | ClosePath deriving (Show, Eq, Enum, Bounded)

data Command = Command
  { _cmd :: CommandA
  , _count :: Int
  }
  deriving (Show, Eq)

makeLenses ''Command

data GeoAction = GeoAction
  { _command :: Command
  , _parameters :: [Point]
  }
  deriving (Show, Eq)

makeLenses ''GeoAction

coordsOrigin :: Point
coordsOrigin = 0.0 D.^& 0.0

-- command:
-- 3 bits
toCommandA :: Int -> CommandA
toCommandA 1 = MoveTo -- [001]
toCommandA 2 = LineTo -- [010]
toCommandA _ = ClosePath -- [111]

decodeCommand :: Int -> Command
decodeCommand c =
  Command
    { _cmd = cType
    , _count = paramC
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
parametersCount = ap ((*) . forAction . _cmd) _count
  where
    forAction :: CommandA -> Int
    forAction MoveTo = 2
    forAction LineTo = 2
    forAction _ = 0

-- https://protobuf.dev/programming-guides/encoding/#signed-ints
decodeParam :: Int -> Double
decodeParam p = int2Double $ (p `shiftR` 1) `xor` (-(p .&. 1))

splitCommands :: [Int] -> [[Int]]
splitCommands [] = []
splitCommands c =
  let (taken, rest) = splitAt toBeSplitted c
   in if null taken then [] else taken : splitCommands rest
  where
    toBeSplitted = parametersCount (decodeCommand $ head c) + 1

tuplify :: [a] -> [D.P2 a]
tuplify [] = []
tuplify [_] = error "cannot tuplify single emelent"
tuplify (x : x' : xs) = x D.^& x' : tuplify xs

splitAtMove :: [GeoAction] -> [[GeoAction]]
splitAtMove xs = filter (not . null) $ f xs []
  where
    f [] agg = [agg]
    f (y : ys) agg =
      if ((MoveTo ==) . _cmd . _command) y
        then agg : f ys [y]
        else f ys (agg ++ [y])

sumTuple :: (Num a) => D.P2 a -> D.P2 a -> D.P2 a
sumTuple p1 p2 = D.p2 $ sum' (D.unp2 p1) (D.unp2 p2)
  where
    sum' (x, y) (x', y') = (x + x', y + y')

shoelaceStep :: (Num a) => D.P2 a -> D.P2 a -> a
shoelaceStep p1 p2 = step' (D.unp2 p1) (D.unp2 p2)
  where
    step' (x, y) (x', y') = (x * y') - (y * x')

-- A single polygon
data SPolygon = SPolygon
  { _pMoveTo :: GeoAction
  , _pLineTo :: GeoAction
  , _pClosePath :: GeoAction
  }
  deriving (Show, Eq)

makeLenses ''SPolygon

type Inner = [SPolygon]

type Outer = SPolygon

type MPolygon = (Outer, Inner)

data PolygonG = SinglePolygon SPolygon | MultiPolygon MPolygon deriving (Show)

makeLenses ''PolygonG

data LineG = LineG
  { _lMoveTo :: GeoAction
  , _lLineTo :: GeoAction
  }
  deriving (Show, Eq)

makeLenses ''LineG

data PointG = PointG
  { _pMoveT :: GeoAction
  }
  deriving (Show, Eq)

makeLenses ''PointG
