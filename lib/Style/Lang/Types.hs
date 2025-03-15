{-# LANGUAGE DeriveGeneric #-}

module Style.Lang.Types where

import Data.Colour
import Data.Scientific
import qualified Data.Text.Lazy as T
import GHC.Generics (Generic)

type SNum = Maybe Scientific

type SColor = Maybe (AlphaColour Double)

type SString = Maybe T.Text

type SBool = Maybe Bool

data SData
  = DNum SNum
  | DString SString
  | DBool SBool
  | DColor SColor
  | DArray [SData]
  deriving (Show, Generic, Eq)
