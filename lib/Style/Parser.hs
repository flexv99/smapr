{-# LANGUAGE OverloadedStrings #-}

module Style.Parser where

import qualified Data.Text.Internal.Lazy as T
import qualified Data.Text.Lazy as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void


type Parser = Parsec
  Void  -- The type for custom error messages. We have none, so use `Void`.
  T.Text -- The input stream type.

lexeme :: Parser a -> Parser a
lexeme = L.lexeme (skipMany (spaceChar <|> char ','))

-- parse snake case property names
snakeCaseChar :: Parser Char
snakeCaseChar = alphaNumChar <|> char '_'

brackets :: Parser a -> Parser a
brackets = between (char '[' >> space) (char ']' >> space)

-- Parse a string enclosed in double quotes
quotedString :: Parser T.Text
quotedString = label "property" $ between (char '"') (char '"') $ do
  propName <- many snakeCaseChar 
  pure $ T.pack propName

innerArrayParser :: Parser [T.Text]
innerArrayParser = brackets (quotedString `sepBy` (char ',' >> space))

-- test: parseTest literal "[\"literal\", [\"a\", \"b\", \"c\"]]"
literal :: Parser [T.Text]
literal = label "literal" $ do
  _ <- char '[' >> space
  key <- quotedString
  _ <- char ',' >> space
  array <- innerArrayParser
  _ <- char ']' >> space
  if key /= T.pack "literal" then fail "Expected \"literal\" as the key" else return array
  

