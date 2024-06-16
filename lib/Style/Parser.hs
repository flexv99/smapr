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

pKeyword :: T.Text -> Parser T.Text
pKeyword keyword = label ("property_key: " ++ T.unpack keyword) $
  between (char '"' >> space) (char '"' >> space) $ 
  lexeme (string keyword <* notFollowedBy alphaNumChar)

-- test: parseTest literal "[\"literal\", [\"a\", \"b\", \"c\"]]"
literal :: Parser [SType]
literal = label "literal" $ do
  _ <- char '[' >> space
  key <- pKeyword "literal"
  _ <- char ',' >> space
  array <- pArray
  _ <- char ']' >> space
  return array

