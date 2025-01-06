module Style.Lang.Token where

data ColorToken
  = THex3
  | THex6
  | TRgb
  | TRgba
  | THsl
  | THsla
  | TName
  | CInterpolate
  | CPoly PolyToken
  deriving (Eq, Show)

data NumToken
  = Plus
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
  | TextAt
  | SPoly PolyToken
  deriving (Eq, Show)

data BoolToken
  = Equality
  | Greater
  | GreaterEq
  | Less
  | LessEq
  | In
  | Negated BoolToken
  | BPoly PolyToken
  deriving (Eq, Show)

data PolyToken
  = Get
  | At
  | PNum NumToken
  | PString StringToken
  | PBool BoolToken
  | PColor ColorToken
  deriving (Eq, Show)
