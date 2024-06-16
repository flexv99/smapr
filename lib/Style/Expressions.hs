{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Style.Expressions where

import GHC.Generics (Generic)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Text.Lazy as T
import Style.Parser

--- LOOKUP:
-- at: Retrieves an item from an array.
-- ["at", value, number]: value
data SAt a = SAt { array :: [a]
                 , index :: Int
                 } deriving (Show, Generic, Eq)

atId :: T.Text
atId = "at"

atP :: Parser (SAt SType)
atP = label (show atId) $
  betweenSquareBrackets $ do
    key <- pKeyword atId
    lexeme (char ',')
    value <- literal
    lexeme (char ',')
    idx <- L.decimal
    return SAt {array = value, index = idx}

-- in: Determines whether an item exists in an array or a substring exists in a string.
-- ["in", value, value]: boolean
data SIn a = SIn { object :: a
                 , item :: a
                 } deriving (Show, Generic, Eq)

inId :: T.Text
inId = "in"

inP :: Parser (SIn SType)
inP = label (show inId) $
  betweenSquareBrackets $ do
    key <- pKeyword inId
    lexeme (char ',')
    obj <- pAtom
    lexeme (char ',')
    itm <- pAtom
    return SIn { object = obj, item = itm }

-- index-of: Returns the first position at which an item can be found in an array or a substring can be found in a string,
-- or -1 if the input cannot be found. Accepts an optional index from where to begin the search.
-- ["index-of", value, value, number?]: number
data SIndexOf a = SIndexOf { lookupItem :: a
                           , items :: [a]
                           , startIndex :: Maybe SType
                           } deriving (Show, Generic, Eq)

indexOfId :: T.Text
indexOfId = "index-of"

indexOfP :: Parser (SIndexOf SType)
indexOfP = label (show indexOfId) $
  betweenSquareBrackets $ do
    key <- pKeyword indexOfId
    lexeme (char ',')
    lookup <- pAtom
    lexeme (char ',')
    onItems <- pArray
    start <- optional $ do
      (lexeme (char ','))
      givenIdx <- pInteger
      return givenIdx
    return SIndexOf { lookupItem = lookup, items = onItems, startIndex = start }

-- Retrieves a property value from the current feature's properties,
-- or from another object if a second argument is provided.
-- Returns null if the requested property is missing.
-- ["get", string]: value
newtype SGet = SGet { runGet :: SType } deriving (Show, Generic, Eq)

getId :: T.Text
getId = "get"

-- Parse a getter expression
getP :: Parser SGet
getP = label (show getId) $
  betweenSquareBrackets $ do
    key <- pKeyword getId
    lexeme (char ',')
    value <- pAtom
    return (SGet value)
