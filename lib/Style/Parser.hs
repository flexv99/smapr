{-# LANGUAGE OverloadedStrings #-}

module Style.Parser where

import qualified Data.Text.Internal.Lazy as T
import qualified Data.Text.Lazy as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Style.Types


skipComma :: Parser a -> Parser a
skipComma = L.lexeme (skipMany (spaceChar <|> char ','))

-- Parse a string enclosed in double quotes
quotedString :: Parser T.Text
quotedString = label "property" $ between (char '"') (char '"') $ do
  propName <- many snakeCaseChar 
  pure $ T.pack propName

-- test: parseTest literal "[\"literal\", [\"a\", \"b\", \"c\"]]"
literal :: Parser [SType]
literal = label "literal" $ do
  _ <- char '[' >> space
  key <- quotedString
  _ <- char ',' >> space
  array <- pArray
  _ <- char ']' >> space
  if key /= T.pack "literal" then fail "Expected \"literal\" as the key" else return array

