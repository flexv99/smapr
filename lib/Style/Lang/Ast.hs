{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Style.Lang.Ast (SExpr (..), InterpolationType (..), OrdType (..)) where

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
  -- | numeric interpolate expr
  InterpolateNumE ::
    InterpolationType ->
    SExpr SNum ->
    [(SExpr SNum, SNum)] ->
    SExpr SNum
  -- | Zoom
  FzoomE :: SExpr SNum
  -- | index of element in list
  IndexOfListE :: SData -> [SData] -> SExpr SNum
  -- | index of element in string
  IndexOfStringE :: SString -> SString -> SExpr SNum
  -- | length of array
  LengthOfListE :: [SData] -> SExpr SNum
  -- | length of string
  LengthOfStringE :: SString -> SExpr SNum
  -- | string literal
  StringE :: SString -> SExpr SString
  -- | cast SData to SString
  StringCastE :: SExpr SData -> SExpr SString
  -- | char at index
  TextAtE :: SExpr SString -> SExpr SNum -> SExpr SString
  -- | to upper
  UpcaseE :: SExpr SString -> SExpr SString
  -- | to lower
  DowncaseE :: SExpr SString -> SExpr SString
  -- | string concatenation
  ConcatE :: SExpr SString -> SExpr SString -> SExpr SString
  -- | Geometry type expression for a given feature
  FgeometryE :: SExpr SString
  -- | bool literal
  BoolE :: SBool -> SExpr SBool
  -- | bool negation
  Negation :: SExpr SBool -> SExpr SBool
  -- | cast SData to SBool
  BoolCastE :: SExpr SData -> SExpr SBool
  -- | equality
  EqE :: SExpr SData -> SExpr SData -> SExpr SBool
  -- | < <= >=
  OrdNumE :: OrdType -> SExpr SNum -> SExpr SNum -> SExpr SBool
  -- | ordering strings
  OrdStringE :: OrdType -> SExpr SString -> SExpr SString -> SExpr SBool
  -- | color literal
  ColorE :: SColor -> SExpr SColor
  -- | color interpolation
  InterpolateColorE ::
    InterpolationType ->
    SExpr SNum ->
    [(SExpr SNum, SColor)] ->
    SExpr SColor
  -- | list literal
  ListE :: (Show a) => [a] -> SExpr [a]
  -- POLY

  -- | SData literal
  SDataE :: SData -> SExpr SData
  -- | Get value from feature tags
  FgetE :: SString -> SExpr SData
  -- | element at index
  AtE :: SExpr [SData] -> SExpr SNum -> SExpr SData

deriving instance Show (SExpr a)

data InterpolationType
  = Linear
  | Exponential SNum
  | CubicBezier SNum SNum SNum SNum
  deriving (Show, Eq)

data OrdType
  = OLess
  | OLessEq
  | OGreater
  | OGreaterEq
  deriving (Eq, Show)
