module Style.Lang.Parser where

import Control.Monad
import Style.Lang.Ast
import Style.Lang.Lex
import Style.Lang.Token
import Style.Lang.Types
import Text.Megaparsec
import Text.Megaparsec.Char

numExprP :: Parser (SExpr SNum)
numExprP = NumE <$> pNum

arithmethicP :: Parser (SExpr SNum)
arithmethicP = betweenSquareBrackets $ do
  arithOp <- betweenDoubleQuotes numSymbol
  _ <- char ',' >> space
  parserForOP arithOp
  where
    parserForOP :: NumToken -> Parser (SExpr SNum)
    parserForOP Plus = AddE <$> numExprP `sepBy` (char ',' >> space)
    parserForOP Minus = SubE <$> argWithComma <*> numExprP
    parserForOP Div = DivE <$> argWithComma <*> numExprP
    parserForOP Multi = ProdE <$> numExprP `sepBy` (char ',' >> space)
    argWithComma :: Parser (SExpr SNum)
    argWithComma = do
      val <- numExprP
      _ <- char ',' >> space
      return val
