module Style.Lang.Token where

data ColorToken
  = THex3
  | THex6
  | TRgb
  | TRgba
  | THsl
  | THsla
  | TName
  deriving (Eq, Show)

data NumToken
  = Plus
  | Minus
  | Div
  | Multi
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
  | Negated BoolToken
  | BPoly PolyToken
  deriving (Eq, Show)

data PolyToken
  = Get
  | At
  deriving (Eq, Show)
