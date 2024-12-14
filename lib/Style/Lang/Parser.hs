{-# LANGUAGE MonoLocalBinds #-}

module Style.Lang.Parser
  ( numExprP,
    stringExprP,
    polyExprP,
  )
where

import Control.Monad
import Style.Lang.Ast
import Style.Lang.Lex
import Style.Lang.Token
import Style.Lang.Types
import Text.Megaparsec
import Text.Megaparsec.Char

--------------------------------------------------------------------------------
-- NUMERIC Functions
--------------------------------------------------------------------------------

numExprP :: Parser (SExpr SNum)
numExprP =
  (NumE <$> pNum)
    <|> betweenSquareBrackets
      ( do
          op <- betweenDoubleQuotes numSymbol
          _ <- optional (char ',' >> space)
          numOpParser op
      )

numOpParser :: NumToken -> Parser (SExpr SNum)
numOpParser Plus = AddE <$> numExprP `sepBy` (char ',' >> space)
numOpParser Minus = do
  arg <- numExprP
  _ <- char ',' >> space
  SubE arg <$> numExprP
numOpParser Multi = ProdE <$> numExprP `sepBy` (char ',' >> space)
numOpParser Div = do
  arg <- numExprP
  _ <- char ',' >> space
  DivE arg <$> numExprP
numOpParser (NPoly n) = NumCastE <$> polyOpParser n

--------------------------------------------------------------------------------
-- STRING Functions
--------------------------------------------------------------------------------

stringExprP :: Parser (SExpr SString)
stringExprP =
  (StringE <$> pString)
    <|> betweenSquareBrackets
      ( do
          op <- betweenDoubleQuotes stringSymbol
          _ <- optional (char ',' >> space)
          stringOpParser op
      )

stringOpParser :: StringToken -> Parser (SExpr SString)
stringOpParser GeometryType = return FgeometryE
stringOpParser TextAt = do
  txt <- stringExprP
  _ <- char ',' >> space
  TextAtE txt <$> numExprP
stringOpParser (SPoly n) = StringCastE <$> polyOpParser n

--------------------------------------------------------------------------------
-- POLYMORPHIC Functions
--------------------------------------------------------------------------------

polyExprP :: Parser (SExpr SData)
polyExprP =
  betweenSquareBrackets
    ( do
        op <- betweenDoubleQuotes polySymbol
        _ <- optional (char ',' >> space)
        polyOpParser op
    )

polyOpParser :: PolyToken -> Parser (SExpr SData)
polyOpParser Get = FgetE <$> pString
polyOpParser At = do
  lst <- ListE <$> pArray
  _ <- char ',' >> space
  AtE lst <$> numExprP
