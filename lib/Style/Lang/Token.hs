module Style.Lang.Token (ColorToken (..)) where

data ColorToken
  = THex3
  | THex6
  | TRgb
  | TRgba
  | THsl
  | THsla
  | TName
  deriving (Eq)
