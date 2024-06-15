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


newtype SGet = SGet { runGet :: T.Text } deriving (Show, Generic)
newtype SAt  = SAt { runAt :: T.Text } deriving (Show, Generic)


-- Parse a getter expression
getP :: Parser SGet
getP = label "get" $ between (char '[') (char ']') $ do
    key <- quotedString
    lexeme (char ',')
    value <- quotedString
    if key == "get"
        then return (SGet value)
        else fail "Expected \"get\" as the key"

atP :: Parser SAt
atP = label "at" $ between (char '[') (char ']') $ do
  key <- quotedString
  lexeme (char ',')
  value <- quotedString
  if key == "at"
        then return (SAt value)
        else fail "Expected \"at\" as the key"
