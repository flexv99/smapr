{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Style.Expressions.Types where

import GHC.Generics (Generic)
import qualified Data.Text.Internal.Lazy as T
import qualified Data.Text.Lazy as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import qualified Data.Aeson.Types as A

data Expression
  = ExprGet T.Text
  | ExprRGB Expression Expression Expression
  | ExprSub Expression Expression
  | ExprNumber Double
  deriving (Show, Generic)

type Parser = Parsec Void T.Text

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: T.Text -> Parser T.Text
symbol = L.symbol sc

stringLiteral :: Parser T.Text
stringLiteral = T.pack <$> (char '"' *> manyTill L.charLiteral (char '"'))

number :: Parser Double
number = lexeme L.float

expressionParser :: Parser Expression
expressionParser = lexeme $ do
  _ <- symbol "["
  expr <- choice
    [ ExprGet <$> (symbol "\"get\"" *> stringLiteral)
    , do
        _ <- symbol "\"rgb\""
        r <- expressionParser
        g <- expressionParser
        ExprRGB r g <$> expressionParser
    , do
        _ <- symbol "\"-\""
        a <- expressionParser
        ExprSub a <$> expressionParser
    , ExprNumber <$> number
    ]
  _ <- symbol "]"
  return expr

-- TODO properly parse valid json, at the moment it does not parse commas:
-- parseTest expressionParser "[\"get\"\"rgba(130, 204, 101, 0.8)\"]"
-- this should NOT be parsable!

instance A.FromJSON Expression where
  parseJSON = A.withArray "Expression" $ \arr ->
    case parse (expressionParser <* eof) "" (T.pack $ show arr) of
      Left err -> fail $ errorBundlePretty err
      Right expr -> return expr
