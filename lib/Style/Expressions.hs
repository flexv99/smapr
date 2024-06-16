{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Style.Expressions where

import GHC.Generics (Generic)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Text.Lazy as T
import Style.Types
import Style.Parser

-- Retrieves an item from an array.
data SAt a = SAt { array :: [a]
                 , index :: Int
                 } deriving (Show, Generic, Eq)

-- Retrieves a property value from the current feature's properties,
-- or from another object if a second argument is provided.
-- Returns null if the requested property is missing.
newtype SGet = SGet { runGet :: SType } deriving (Show, Generic, Eq)


atP :: Parser (SAt SType)
atP = label "at" $ between (char '[' >> space) (char ']' >> space) $ do
  key <- pKeyword "at"
  lexeme (char ',')
  value <- literal
  lexeme (char ',')
  idx <- L.decimal
  return SAt {array = value, index = idx}

-- Parse a getter expression
getP :: Parser SGet
getP = label "get" $ between (char '[' >> space) (char ']' >> space) $ do
    key <- pKeyword "get"
    lexeme (char ',')
    value <- pAtom
    return (SGet value)
