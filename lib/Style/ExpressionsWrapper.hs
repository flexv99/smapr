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
  Negation   :: FeatureExpr (SBool s) -> FeatureExpr (SBool b)
  -- | all expr
  FallE      :: [FeatureExpr (SBool b)] -> FeatureExpr (SBool b)
  -- | filter by equality
  FeqE       :: FilterBy -> SType -> FeatureExpr (SBool b)
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
  StringE :: T.Text -> IsoExpr (SString s)
  -- | bool-value
  BoolE   :: Bool -> IsoExpr (SBool b)
  -- | int literal
  IntE    :: Int -> IsoExpr (SNum (SInt i))
  -- | double literal
  DoubleE :: Double -> IsoExpr (SNum (SDouble d))
  -- | list literal
  ArrayE  :: SType -> IsoExpr (SArray a)
  -- | addition
  AddE    :: SType -> IsoExpr a
  -- | product
  ProdE   :: SType -> IsoExpr a
  -- | subtraction
  SubE    :: SType -> SType -> IsoExpr a
  -- | division
  DivE    :: SType -> SType -> IsoExpr a
  -- | check for equaliy on polymorphic types
  EqE     :: WrappedExpr -> WrappedExpr -> IsoExpr (SBool b)
  -- | element at index
  AtE    :: SType -> IsoExpr (SNum (SInt i)) -> IsoExpr a

deriving instance Show (IsoExpr res)

-- | runtime representation
--   mainly useful for parsing
data WrappedExpr where
  StringExpr   :: IsoExpr (SString n)      -> WrappedExpr
  NumExpr      :: IsoExpr (SNum a)         -> WrappedExpr
  BoolExpr     :: IsoExpr (SBool b)        -> WrappedExpr
  ArrayExpr    :: IsoExpr (SArray a)       -> WrappedExpr
  FStringExpr  :: FeatureExpr (SString n)  -> WrappedExpr
  FNumExpr     :: FeatureExpr (SNum a)     -> WrappedExpr
  FBoolExpr    :: FeatureExpr (SBool b)    -> WrappedExpr
  FArrayExpr   :: FeatureExpr (SArray a)   -> WrappedExpr
  

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
  wrap  :: IsoExpr a     -> WrappedExpr
  fwrap :: FeatureExpr a -> WrappedExpr

instance KnownResType (SString b) where
  wrap  = StringExpr
  fwrap = FStringExpr

instance KnownResType (SNum a) where
  wrap  = NumExpr
  fwrap = FNumExpr

instance KnownResType (SBool b) where
  wrap  = BoolExpr
  fwrap = FBoolExpr

instance KnownResType (SArray a) where
  wrap  = ArrayExpr
  fwrap = FArrayExpr
