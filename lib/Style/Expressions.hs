{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Style.Expressions where

import GHC.Generics (Generic)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Text as T
import Style.Types

type Parser = Parsec
  Void  -- The type for custom error messages. We have none, so use `Void`.
  T.Text -- The input stream type. Let's use `String` for now, but for better performance, you might want to use `Text` or `ByteString`.

newtype SGet = SGet { runGet :: T.Text } deriving (Show, Generic)

-- A lexer that consumes spaces and commas
lexeme :: Parser a -> Parser a
lexeme = L.lexeme (skipMany (spaceChar <|> char ','))

-- Parse a string enclosed in double quotes
quotedString :: Parser T.Text
quotedString = char '"' >> T.pack <$> manyTill L.charLiteral (char '"')

-- Parse a getter expression
getterExpression :: Parser SGet
getterExpression = between (lexeme (char '[')) (lexeme (char ']')) $ do
    key <- quotedString
    lexeme (char ',')
    value <- quotedString
    if key == "get"
        then return (SGet value)
        else fail "Expected \"get\" as the key"

-- test: parseTest getterExpression (toStrict $ encodeToLazyText ["get", "someProperyt"])
