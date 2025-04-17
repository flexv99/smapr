module Style.Lang.Token where

data ColorToken
  = THex
  | TRgb
  | TRgba
  | THsl
  | THsla
  | TName
  | CInterpolate
  | CPoly PolyToken
  deriving (Eq, Show)

data NumToken
  = Number
  | Plus
  | Minus
  | Div
  | Multi
  | NInterpolate
  | Zoom
  | IndexOf
  | Length
  | NPoly PolyToken
  deriving (Eq, Show)

data StringToken
  = GeometryType
  | Upcase
  | Downcase
  | Concat
  | SPoly PolyToken
  deriving (Eq, Show)

data BoolToken
  = Boolean
  | Neg
  | Equality
  | Greater
  | GreaterEq
  | Less
  | LessEq
  | In
  | Has
  | All
  | Any
  | Negated BoolToken
  | BPoly PolyToken
  deriving (Eq, Show)

data ArrayToken
  = Array
  | APoly PolyToken
  deriving (Eq, Show)

data PolyToken
  = Get
  | At
  | Match
  | Case
  | Step
  | PNum NumToken
  | PString StringToken
  | PBool BoolToken
  | PColor ColorToken
  | PArray ArrayToken
  deriving (Eq, Show)
