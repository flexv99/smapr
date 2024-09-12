{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Style.ExpressionsWrapper where

import Data.Kind (Type)
import qualified Data.Text.Lazy as T
import Style.Parser

-- | AST representation of expressions requiring feature context
data FeatureExpr :: SType -> Type where
  -- | getter on feature properties
  FgetE :: SType -> FeatureExpr a
  -- | Geometry type expression for a given feature
  FgeometryE :: FeatureExpr (SString s)
  -- | Zoom
  FzoomE :: FeatureExpr (SNum a)

deriving instance Show (FeatureExpr res)

-- | AST representation of map libre's style spec expressions
--   value-type the expression is representing (see 'evalExpr')
-- representing expressions that don't required layer or feature context
data IsoExpr :: SType -> Type where
  -- | string literal
  StringE :: T.Text -> IsoExpr (SString s)
  -- | int literal
  IntE :: Int -> IsoExpr (SNum i)
  -- | double literal
  DoubleE :: Double -> IsoExpr (SNum i)
  -- | Num literal
  NumE :: INum -> IsoExpr (SNum n)
  -- | addition
  AddE :: [ArgType (SNum n)] -> IsoExpr (SNum n)
  -- | product
  ProdE :: [ArgType (SNum n)] -> IsoExpr (SNum n)
  -- | subtraction
  SubE :: ArgType (SNum n) -> ArgType (SNum n) -> IsoExpr (SNum n)
  -- | division
  DivE :: ArgType (SNum n) -> ArgType (SNum n) -> IsoExpr (SNum n)
  -- | bool literal
  BoolE :: Bool -> IsoExpr (SBool b)
  -- | negation of bool expressions
  Negation :: IsoExpr (SBool s) -> IsoExpr (SBool b)
  -- | check for equaliy on polymorphic types
  EqE :: WrappedExpr -> WrappedExpr -> IsoExpr (SBool b)
  -- | < <= > >=
  OrdE :: OrdType -> ArgType (SNum n) -> ArgType (SNum n) -> IsoExpr (SBool b)
  -- | checks if element is in an array or string
  InE :: WrappedExpr -> WrappedExpr -> IsoExpr (SBool b)
  -- | all expr
  AllE :: [ArgType (SBool b)] -> IsoExpr (SBool b)
  -- | list literal
  ArrayE :: SType -> IsoExpr (SArray a)
  -- | Color literal
  ColorE :: SType -> IsoExpr (SColor c)
  -- | match expr
  MatchE :: WrappedExpr -> MatchArg -> IsoExpr a
  -- | element at index
  AtE :: SType -> ArgType (SNum (SInt i)) -> IsoExpr a
  -- | interpolate expr
  InterpolateE ::
    InterpolationType ->
    ArgType (SNum i) ->
    [(SType, WrappedExpr)] ->
    IsoExpr a

deriving instance Show (IsoExpr res)

-- | representation of argument type
-- | as the evaluator needs to know in which context
-- | the expression stands
data ArgType t where
  IsoArg :: IsoExpr t -> ArgType t
  FeatureArg :: FeatureExpr t -> ArgType t

deriving instance Show (ArgType t)

-- | runtime representation
-- | mainly useful for parsing
data WrappedExpr where
  StringExpr :: ArgType (SString n) -> WrappedExpr
  NumExpr :: ArgType (SNum a) -> WrappedExpr
  BoolExpr :: ArgType (SBool b) -> WrappedExpr
  ArrayExpr :: ArgType (SArray a) -> WrappedExpr
  ColorExpr :: ArgType (SColor c) -> WrappedExpr

deriving instance Show WrappedExpr

-- | helps wrapping 'Expr' to the right
--   'WrappedExpr' constructor
--
-- >>> wrap (IntE 42)
-- IntExpr (IntE 42)
--
-- >>> wrap (IsNullE (IntE 42))
-- BoolExpr (IsNullE (IntE 42))
class KnownResType a where
  wrap :: ArgType a -> WrappedExpr

instance KnownResType (SString b) where
  wrap = StringExpr

instance KnownResType (SNum a) where
  wrap = NumExpr

instance KnownResType (SBool b) where
  wrap = BoolExpr

instance KnownResType (SArray a) where
  wrap = ArrayExpr

instance KnownResType (SColor c) where
  wrap = ColorExpr

-- Helper types
type ToBeMatched = (SType, SType)

newtype MatchArg = MatchArg ([ToBeMatched], SType) deriving (Show, Eq)

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
