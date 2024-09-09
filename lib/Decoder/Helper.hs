{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Decoder.Helper where

import Control.Lens
import Control.Monad
import qualified Data.Aeson as A
import Data.Bits
import GHC.Float

type Point = (Double, Double)

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

instance A.ToJSON GeoAction where
  toJSON (GeoAction command' parameters') =
    A.object ["command" A..= command', "parameters" A..= parameters']

instance A.ToJSON Command where
  toJSON (Command cmd' count') =
    A.object ["cmd" A..= show cmd', "count" A..= count']

coordsOrigin :: Point
coordsOrigin = (0.0, 0.0)


-- command:
-- 3 bits
toCommandA :: Int -> CommandA
toCommandA 1 = MoveTo -- [001]
toCommandA 2 = LineTo -- [010]
toCommandA _ = ClosePath -- [111]

decodeCommand :: Int -> Command
decodeCommand c =
  Command
    { _cmd = cType,
      _count = paramC
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

tuplify :: [a] -> [(a, a)]
tuplify [] = []
tuplify [_] = error "cannot tuplify single emelent"
tuplify (x : x' : xs) = (x, x') : tuplify xs

splitAtMove :: [GeoAction] -> [[GeoAction]]
splitAtMove xs = filter (not . null) $ f xs []
  where
    f [] agg = [agg]
    f (y : ys) agg =
      if ((MoveTo ==) . _cmd . _command) y
        then agg : f ys [y]
        else f ys (agg ++ [y])

sumTuple :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
sumTuple (x, y) (x', y') = (x + x', y + y')

data PolygonG = PolygonG
  { _pMoveTo :: GeoAction
  , _pLineTo :: GeoAction
  , _pClosePath :: GeoAction
  }
  deriving (Show, Eq)

makeLenses ''PolygonG

instance A.ToJSON PolygonG where
  toJSON (PolygonG pMoveTo' pLineTo' pClosePath') =
        A.object ["move_to" A..= pMoveTo', "line_to" A..= pLineTo', "close_path" A..= pClosePath']

  toEncoding (PolygonG pMoveTo' pLineTo' pClosePath') =
        A.pairs $ "move_to" A..= pMoveTo' <> "line_to" A..= pLineTo' <> "close_path" A..= pClosePath'

data LineG = LineG
  { _lMoveTo :: GeoAction
  , _lLineTo :: GeoAction
  }
  deriving (Show, Eq)

makeLenses ''LineG

instance A.ToJSON LineG where
  toJSON (LineG pMoveTo' pLineTo') =
        A.object ["move_to" A..= pMoveTo', "line_to" A..= pLineTo']

  toEncoding (LineG pMoveTo' pLineTo') =
        A.pairs $ "move_to" A..= pMoveTo' <> "line_to" A..= pLineTo'

data PointG = PointG
  { _pMoveT :: GeoAction
  }
  deriving (Show, Eq)

makeLenses ''PointG
