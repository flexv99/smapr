{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Style.Lang.Ast (SExpr (..)) where

import Style.Lang.Types

-- | AST representation of map libre's style spec expressions
data SExpr a where
  -- | num literal
  NumE :: SNum -> SExpr SNum
  -- | cast SData to SNum
  NumCastE :: SExpr SData -> SExpr SNum
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
  -- | cast SData to SString
  StringCastE :: SExpr SData -> SExpr SString
  -- | char at index
  TextAtE :: SExpr SString -> SExpr SNum -> SExpr SString
  -- | Geometry type expression for a given feature
  FgeometryE :: SExpr SString
  -- | bool literal
  BoolE :: SBool -> SExpr SBool
  -- | cast SData to SBool
  BoolCastE :: SExpr SData -> SExpr SBool
  -- | color literal
  ColorE :: SColor -> SExpr SColor
  -- | list literal
  ListE :: (Show a) => [a] -> SExpr [a]
  -- POLY

  -- | Get value from feature tags
  FgetE :: SExpr SString -> SExpr SData
  -- | element at index
  AtE :: SExpr [SData] -> SExpr SNum -> SExpr SData

deriving instance Show (SExpr a)
