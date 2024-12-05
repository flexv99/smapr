{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Style.Lang.Ast (SExpr (..)) where

import Style.Lang.Types

-- | AST representation of map libre's style spec expressions
--   value-type the expression is representing (see 'evalExpr')
-- representing expressions that don't required layer or feature context
data SExpr a where
  -- | num literal
  NumE :: SNum -> SExpr SNum
  -- | addition
  AddE :: [SExpr SNum] -> SExpr SNum
  -- | product
  ProdE :: [SExpr SNum] -> SExpr SNum
  -- | subtraction
  SubE :: SExpr SNum -> SExpr SNum -> SExpr SNum
  -- | division
  DivE :: SExpr SNum -> SExpr SNum -> SExpr SNum
  -- | string literal
  StringE :: SString -> SExpr SString
  -- | bool literal
  BoolE :: SBool -> SExpr SBool
  -- | color literal
  ColorE :: SColor -> SExpr SColor

deriving instance Show (SExpr a)
