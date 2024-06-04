{-# LANGUAGE DeriveGeneric #-}

module Style.Types where

import GHC.Generics (Generic)
-- import qualified Data.Text.Internal.Lazy as T
-- import qualified Data.Text.Lazy as T
-- import Text.Megaparsec
-- import Text.Megaparsec.Char
-- import qualified Text.Megaparsec.Char.Lexer as L
-- import Data.Void
-- import qualified Data.Aeson.Types as A
-- import Data.Colour

-- Here are defined the types being used in the style spec
-- reference: https://github.com/maplibre/maplibre-style-spec/blob/main/src/expression/types.ts#L133
-- Docs: https://maplibre.org/maplibre-style-spec/expressions/#types

{- 
Supported datatypes:
missing ones:
'null'
'color'
'object'
'value'
'error'
'collator'
'formatted'
'padding'
'resolvedImage'
'variableAnchorOffsetCollection'
-}

newtype StylesArray = StylesArray
  { getArray :: ([SType], Int, String) } deriving (Show, Generic)

data SType
  = SInteger Int
  | SDouble  Double
  | SString  String
  | SBool    Bool
  | STypeOf  String
  | SArray   StylesArray
  deriving (Show, Generic)
