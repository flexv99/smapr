{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}

module Style.ExpressionsWrapper where

import Data.Kind (Type)
import Proto.Vector_tile.Tile.Feature (Feature(..))
import Proto.Vector_tile.Tile.Layer (Layer(..))
import qualified Data.Text.Lazy as T
import Style.Parser

-- | AST representation of filter expressions
data FeatureExpr :: SType -> Type where
  StringFe   :: T.Text -> FeatureExpr (SString s)
  ArrayFe    :: SType -> FeatureExpr (SArray a)
  NegationFe :: FeatureExpr (SBool s) -> FeatureExpr (SBool b)
  -- | in lookup
  FinE       :: FilterBy -> SType -> FeatureExpr (SBool b)
  -- | getter on feature properties
  FgetE      :: SType -> FeatureExpr a
  -- | Geometry type expression for a given feature
  FgeometryE :: FeatureExpr (SString s)

deriving instance Show (FeatureExpr res)

-- | AST representation of map libre's style spec expressions
--   value-type the expression is representing (see 'evalExpr')
-- representing expressions that don't required layer or feature context
data IsoExpr :: SType -> Type where
  -- | string literal
  StringE  :: T.Text -> IsoExpr (SString s)
  -- | bool-value
  BoolE    :: Bool -> IsoExpr (SBool b)
  -- | int literal
  IntE     :: Int -> IsoExpr (SNum (SInt i))
  -- | double literal
  DoubleE  :: Double -> IsoExpr (SNum (SDouble d))
  -- | Num literal
  NumE     :: INum -> IsoExpr (SNum n)
  -- | list literal
  ArrayE   :: SType -> IsoExpr (SArray a)
  -- | negation of bool expressions
  Negation :: IsoExpr (SBool s) -> IsoExpr (SBool b)
  -- | addition
  AddE     :: [WrappedExpr] -> IsoExpr a
  -- | product
  ProdE    :: [WrappedExpr] -> IsoExpr a
  -- | subtraction
  SubE     :: WrappedExpr -> WrappedExpr -> IsoExpr a
  -- | division
  DivE     :: WrappedExpr -> WrappedExpr -> IsoExpr a
  -- | check for equaliy on polymorphic types
  EqE      :: WrappedExpr -> WrappedExpr -> IsoExpr (SBool b)
  -- | element at index
  AtE      :: SType -> IsoExpr (SNum (SInt i)) -> IsoExpr a
  -- | all expr
  AllE    :: [ArgType (SBool b)] -> IsoExpr (SBool b)

deriving instance Show (IsoExpr res)

data ArgType t where
  IsoArg     :: IsoExpr t     -> ArgType t
  FeatureArg :: FeatureExpr t -> ArgType t

deriving instance Show (ArgType t)

-- | runtime representation
--   mainly useful for parsing
data WrappedExpr where
  StringExpr   :: ArgType (SString n)      -> WrappedExpr
  NumExpr      :: ArgType (SNum a)         -> WrappedExpr
  BoolExpr     :: ArgType (SBool b)        -> WrappedExpr
  ArrayExpr    :: ArgType (SArray a)       -> WrappedExpr
  

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
  wrap  :: ArgType a     -> WrappedExpr

instance KnownResType (SString b) where
  wrap  = StringExpr

instance KnownResType (SNum a) where
  wrap  = NumExpr

instance KnownResType (SBool b) where
  wrap  = BoolExpr

instance KnownResType (SArray a) where
  wrap  = ArrayExpr
