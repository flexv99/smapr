{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Style.Types where

import GHC.Generics (Generic)
import qualified Data.Text.Internal.Lazy as T
import qualified Data.Text.Lazy as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import qualified Data.Aeson.Types as A
import Data.Colour
import Control.Monad

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

type Parser = Parsec Void T.Text
-- Void: The type for custom error messages. We have none, so use `Void`.
-- T.Text: The input stream type.


newtype StylesArray = StylesArray
  { getArray :: ([SType], Int, String) } deriving (Show, Generic)

data SType
  = SInteger Int
  | SDouble  Double
  | SString  T.Text
  | SBool    Bool
  -- | STypeOf  String
  -- | SArray   StylesArray
  deriving (Show, Generic, Eq, Ord)

-- the space consumer
sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: T.Text -> Parser T.Text
symbol = L.symbol sc

-- parse snake case property names
snakeCaseChar :: Parser Char
snakeCaseChar = alphaNumChar <|> char '_'

pString :: Parser SType
pString = SString <$> liftM T.pack (
  between (char '"' >> space) (char '"' >> space)
  (lexeme (many snakeCaseChar) <?> "string literal"))

pInteger :: Parser SType
pInteger = SInteger <$> lexeme (L.signed space L.decimal)

pDouble :: Parser SType
pDouble = SDouble <$> lexeme (L.signed space L.float)

pNumber :: Parser SType
pNumber = try pDouble <|> pInteger <?> "number"

pBool :: Parser SType
pBool = label "bool" $ lexeme $
  (SBool False <$ (string "false" *> notFollowedBy alphaNumChar))
  <|> (SBool True <$ (string "true" *> notFollowedBy alphaNumChar))

pAtom :: Parser SType
pAtom = try $ choice
  [ pBool
  , pNumber
  , pString
  ]

pArray :: Parser [SType]
pArray = between (char '[' >> space) (char ']' >> space)
  (pAtom `sepBy` (char ',' >> space))

