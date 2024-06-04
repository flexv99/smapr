{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Style.Expressions where

import GHC.Generics (Generic)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Text.Lazy as T
import Style.Types

type Parser = Parsec
  Void  -- The type for custom error messages. We have none, so use `Void`.
  T.Text -- The input stream type. Let's use `String` for now, but for better performance, you might want to use `Text` or `ByteString`.

newtype SGet = SGet { runGet :: T.Text } deriving (Show, Generic)

-- A lexer that consumes spaces and commas
lexeme :: Parser a -> Parser a
lexeme = L.lexeme (skipMany (spaceChar <|> char ','))

-- parse snake case property names
snakeCaseChar :: Parser Char
snakeCaseChar = alphaNumChar <|> char '_'

-- Parse a string enclosed in double quotes
quotedString :: Parser T.Text
quotedString = label "property" $ between (char '"') (char '"') $ do
  propName <- many snakeCaseChar 
  pure $ T.pack propName

-- Parse a getter expression
getterExpression :: Parser SGet
getterExpression = label "get" $ between (char '[') (char ']') $ do
    key <- quotedString
    lexeme (char ',')
    value <- quotedString
    if key == "get"
        then return (SGet value)
        else fail "Expected \"get\" as the key"

-- test: parseTest getterExpression (encodeToLazyText ["get", "someProperyt"])
