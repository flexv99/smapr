{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Style.Lang.Ast (
  SExpr (..),
  InterpolationType (..),
  NumOrString (..),
  STraversable (..),
  OrdType (..),
)
where

import Style.Lang.Types

-- | AST representation of map libre's style spec expressions
data SExpr a where
  -- | num literal
  NumE :: SNum -> SExpr SNum
  -- | cast SData to SNum
  NumCastE :: SExpr SData -> SExpr SNum
  -- | num expression
  NumberE :: [SExpr SData] -> SExpr SNum
  -- | addition
  AddE :: [SExpr SNum] -> SExpr SNum
  -- | product
  ProdE :: [SExpr SNum] -> SExpr SNum
  -- | subtraction
  SubE :: SExpr SNum -> SExpr SNum -> SExpr SNum
  -- | division
  DivE :: SExpr SNum -> SExpr SNum -> SExpr SNum
  -- | numeric interpolate expr
  InterpolateNumE
    :: InterpolationType
    -> SExpr SNum
    -> [(SExpr SNum, SNum)]
    -> SExpr SNum
  -- | Zoom
  FzoomE :: SExpr SNum
  -- | index of element in list
  IndexOfListE :: SData -> [SData] -> SExpr SNum
  -- | index of element in string
  IndexOfStringE :: SString -> SExpr SString -> SExpr SNum
  -- | length of array
  LengthOfListE :: [SData] -> SExpr SNum
  -- | length of string
  LengthOfStringE :: SExpr SString -> SExpr SNum
  -- | string literal
  StringE :: SString -> SExpr SString
  -- | cast SData to SString
  StringCastE :: SExpr SData -> SExpr SString
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
  OrdE :: OrdType -> NumOrString -> NumOrString -> SExpr SBool
  -- | in
  InE :: SExpr SData -> STraversable -> SExpr SBool
  -- | has
  HasE :: SExpr SString -> SExpr SBool
  -- Has2E :: SExpr SString -> [(SString, SData)] -> SExpr SBool

  -- | all
  AllE :: [SExpr SBool] -> SExpr SBool
  -- | color literal
  ColorE :: SColor -> SExpr SColor
  -- | color interpolation
  InterpolateColorE
    :: InterpolationType
    -> SExpr SNum
    -> [(SExpr SNum, SColor)]
    -> SExpr SColor
  -- | list literal
  ArrE :: [SData] -> SExpr [SData]
  -- array assert expr
  ArrayE :: [SExpr SData] -> SExpr [SData]
  -- | cast sdata to array
  ArrayCastE :: SExpr SData -> SExpr [SData]
  -- POLY

  -- | SData literal
  SDataE :: SData -> SExpr SData
  -- | SData wrappers
  FromNum :: SExpr SNum -> SExpr SData
  FromString :: SExpr SString -> SExpr SData
  FromBool :: SExpr SBool -> SExpr SData
  FromColor :: SExpr SColor -> SExpr SData
  FromArray :: SExpr [SData] -> SExpr SData
  -- | Get value from feature tags
  FgetE :: SExpr SString -> SExpr SData
  -- | element at index
  AtE :: SExpr SNum -> STraversable -> SExpr SData
  -- | match
  MatchE :: SExpr SData -> [(SExpr SData, SExpr SData)] -> SExpr SData -> SExpr SData

deriving instance Show (SExpr a)

data InterpolationType
  = Linear
  | Exponential SNum
  | CubicBezier SNum SNum SNum SNum
  deriving (Show, Eq)

type NumOrString = Either (SExpr SNum) (SExpr SString)

type STraversable = Either [SData] (SExpr SString)

data OrdType
  = OLess
  | OLessEq
  | OGreater
  | OGreaterEq
  deriving (Eq, Show)
