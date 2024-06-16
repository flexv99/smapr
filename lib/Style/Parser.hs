{-# LANGUAGE OverloadedStrings, DeriveGeneric, TypeOperators #-}

module Style.Parser where


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

type Parser = Parsec Void T.Text
-- Void: The type for custom error messages. We have none, so use `Void`.
-- T.Text: The input stream type.


newtype StylesArray = StylesArray
  { getArray :: ([SType], Int, String) } deriving (Show, Generic)

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

data SType
  = SInteger Int
  | SDouble  Double
  | SString  T.Text
  | SBool    Bool
  -- | STypeOf  String
  -- | SArray   StylesArray
  deriving (Show, Generic, Eq, Ord)

--- HELPERS

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

betweenSquareBrackets :: (Token s ~ Char, MonadParsec e s m) => m a -> m a
betweenSquareBrackets = between (char '[' >> space) (char ']' >> space)

betweenDoubleQuotes :: (Token s ~ Char, MonadParsec e s m) => m a -> m a
betweenDoubleQuotes = between (char '"' >> space) (char '"' >> space)

--- PARSER

pString :: Parser SType
pString = SString <$> liftM T.pack (
  betweenDoubleQuotes
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
pArray = betweenSquareBrackets
  (pAtom `sepBy` (char ',' >> space))


skipComma :: Parser a -> Parser a
skipComma = L.lexeme (skipMany (spaceChar <|> char ','))

pKeyword :: T.Text -> Parser T.Text
pKeyword keyword = label ("property_key: " ++ T.unpack keyword) $
  betweenDoubleQuotes $ 
  lexeme (string keyword <* notFollowedBy alphaNumChar)

-- test: parseTest literal "[\"literal\", [\"a\", \"b\", \"c\"]]"

literalId :: T.Text
literalId = "literal"

literal :: Parser [SType]
literal = label (show literalId) $ betweenSquareBrackets $ do
  key <- pKeyword literalId
  _ <- char ',' >> space
  array <- pArray
  return array

