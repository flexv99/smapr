{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Style.ExpressionsWrapper where

import qualified Data.Text.Lazy as T
import Style.ExpressionsContext
import Style.Parser

-- | AST representation of map libre's style spec expressions
--   value-type the expression is representing (see 'evalExpr')
-- representing expressions that don't required layer or feature context
data IsoExpr a where
  -- | string literal
  StringE :: T.Text -> IsoExpr T.Text
  -- | int literal
  IntE :: Int -> IsoExpr Int
  -- | double literal
  DoubleE :: Double -> IsoExpr Double
  -- | num literal
  NumE :: INum -> IsoExpr INum
  -- | addition
  AddE :: [IsoExpr INum] -> IsoExpr INum
  -- | product
  ProdE :: [IsoExpr INum] -> IsoExpr INum
  -- | subtraction
  SubE :: IsoExpr INum -> IsoExpr INum -> IsoExpr INum
  -- | division
  DivE :: IsoExpr INum -> IsoExpr INum -> IsoExpr INum
  -- | bool literal
  BoolE :: Bool -> IsoExpr Bool
  -- | negation of bool expressions
  Negation :: IsoExpr Bool -> IsoExpr Bool
  -- | check for equaliy on polymorphic types
  EqE :: (Show a, SParseable a, Eq a) => IsoExpr a -> IsoExpr a -> IsoExpr Bool
  -- | < <= > >=
  OrdE :: OrdType -> IsoExpr INum -> IsoExpr INum -> IsoExpr Bool
  -- | checks if element is in an array or string
  InE :: (Show a, SParseable a) => IsoExpr a -> IsoExpr a -> IsoExpr Bool
  -- | all expr
  AllE :: [IsoExpr Bool] -> IsoExpr Bool
  -- | list literal
  ArrayE :: (Show a, SParseable a) => [a] -> IsoExpr [a]
  -- | Color literal
  ColorE :: Color -> IsoExpr Color
  -- | match expr
  -- MatchE :: WrappedExpr -> MatchArg -> IsoExpr a
  -- | case expr
  CaseE :: (Show a, SParseable a) => [(IsoExpr Bool, IsoExpr a)] -> IsoExpr a -> IsoExpr a
  -- | element at index
  AtE :: (Show a, SParseable a) => [IsoExpr a] -> IsoExpr INum -> IsoExpr a
  -- | coalesce
  CoalesceE :: (Show a, SParseable a) => [IsoExpr a] -> IsoExpr a
  -- | interpolate expr
  InterpolateE ::
    (Show a, SParseable a, Floating a) =>
    InterpolationType ->
    IsoExpr INum ->
    [(IsoExpr INum, IsoExpr a)] ->
    IsoExpr a
  -- | getter on feature properties
  FgetE :: T.Text -> IsoExpr SType
  -- | Geometry type expression for a given feature
  FgeometryE :: IsoExpr T.Text
  -- | Zoom
  FzoomE :: IsoExpr INum

deriving instance Show (IsoExpr res)

class SParseable a where
  sParse :: Parser (IsoExpr a)
  sEval :: IsoExpr a -> ExpressionContext -> a

-- | runtime representation
-- | mainly useful for parsing
data WrappedExpr where
  StringExpr :: IsoExpr T.Text -> WrappedExpr
  NumExpr :: IsoExpr INum -> WrappedExpr
  BoolExpr :: IsoExpr Bool -> WrappedExpr
  ArrayExpr :: IsoExpr [a] -> WrappedExpr
  ColorExpr :: IsoExpr Color -> WrappedExpr

deriving instance Show WrappedExpr

class KnownResType a where
  wrap :: IsoExpr a -> WrappedExpr

instance KnownResType T.Text where
  wrap = StringExpr

instance KnownResType INum where
  wrap = NumExpr

instance KnownResType Bool where
  wrap = BoolExpr

instance KnownResType [a] where
  wrap = ArrayExpr

instance KnownResType Color where
  wrap = ColorExpr

-- Helper types
-- type ToBeMatched = (a, a)

-- newtype MatchArg = MatchArg ([ToBeMatched], a) deriving (Show, Eq)

data InterpolationType
  = Linear
  | Exponential INum
  | CubicBezier INum INum INum INum
  deriving (Show, Eq)

data FilterBy
  = FTypeOf
  | FId Int
  | FProp T.Text
  deriving (Eq, Show)

data OrdType
  = Less
  | LessEq
  | Greater
  | GreaterEq
  deriving (Eq, Show)
