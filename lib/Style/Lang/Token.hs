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
  deriving (Eq, Show)
