{-# LANGUAGE MonoLocalBinds #-}

module Style.Lang.Parser (
  numExprP,
  stringExprP,
  boolExprP,
  colorExprP,
  polyExprP,
)
where

import qualified Data.Aeson as A
import qualified Data.Text.Lazy as T
import Style.Lang.Ast
import Style.Lang.Lex
import Style.Lang.Token
import Style.Lang.Types
import Text.Megaparsec
import Text.Megaparsec.Char

instance A.FromJSON SData where
  parseJSON (A.Number n) = pure $ DNum $ Just n
  parseJSON (A.Bool b) = pure $ DBool $ Just b
  -- parseJSON (A.Array a) = SArray <$> traverse A.parseJSON (V.toList a)
  parseJSON (A.String s) = pure $ DString $ Just $ T.fromStrict s
  parseJSON a =
    A.withText
      "SData"
      ( \v ->
          case parse pAtom "" (T.fromStrict v) of
            Left err -> fail $ errorBundlePretty err
            Right res -> return res
      )
      a

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
numOpParser NInterpolate = do
  interType <- interpolationTypeP
  _ <- char ',' >> space
  input <- numExprP
  _ <- char ',' >> space
  InterpolateNumE interType input <$> inOutPairs `sepBy` (char ',' >> space)
  where
    inOutPairs = do
      num1 <- numExprP
      _ <- char ',' >> space
      num2 <- pNum
      return (num1, num2)
numOpParser Zoom = return FzoomE
numOpParser IndexOf = do
  elem <- pAtom
  _ <- char ',' >> space
  elems <- pTraversable
  case elems of
    (Left l) -> return $ IndexOfListE elem l
    (Right s) -> return $ IndexOfStringE (unwrapText elem) s
  where
    unwrapText :: SData -> SString
    unwrapText (DString s) = s
    unwrapText _ = Nothing -- for completeness, will newer happen
numOpParser Length = do
  elems <- pTraversable
  case elems of
    (Left l) -> return $ LengthOfListE l
    (Right s) -> return $ LengthOfStringE s
numOpParser Number = NumberE <$> (polyExprP `sepBy1` (char ',' >> space))
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
  -- causes infinite loop on at, as there is a polymorphic one too
  txt <- stringExprP
  _ <- char ',' >> space
  TextAtE txt <$> numExprP
stringOpParser Upcase = UpcaseE <$> stringExprP
stringOpParser Downcase = DowncaseE <$> stringExprP
stringOpParser Concat = do
  s1 <- stringExprP
  _ <- char ',' >> space
  ConcatE s1 <$> stringExprP
stringOpParser (SPoly n) = StringCastE <$> polyOpParser n

--------------------------------------------------------------------------------
-- BOOL Functions
--------------------------------------------------------------------------------

boolExprP :: Parser (SExpr SBool)
boolExprP =
  (BoolE <$> pBool)
    <|> betweenSquareBrackets
      ( do
          op <- betweenDoubleQuotes boolSymbol
          _ <- optional (char ',' >> space)
          boolOpParser op
      )

boolOpParser :: BoolToken -> Parser (SExpr SBool)
boolOpParser (Negated t) = Negation <$> boolOpParser t
boolOpParser Equality = do
  let argsP = SDataE <$> pAtom <|> polyExprP
  v1 <- argsP
  _ <- char ',' >> space
  EqE v1 <$> argsP
boolOpParser Less = OrdE OLess <$> numOrStringP <* (char ',' >> space) <*> numOrStringP
boolOpParser LessEq = OrdE OLessEq <$> numOrStringP <* (char ',' >> space) <*> numOrStringP
boolOpParser Greater = OrdE OGreater <$> numOrStringP <* (char ',' >> space) <*> numOrStringP
boolOpParser GreaterEq = OrdE OGreaterEq <$> numOrStringP <* (char ',' >> space) <*> numOrStringP
boolOpParser In = InE <$> polyExprP <* (char ',' >> space) <*> pTraversable
boolOpParser All = AllE <$> boolExprP `sepBy` (char ',' >> space)
boolOpParser Has = HasE <$> stringExprP
boolOpParser (BPoly t) = BoolCastE <$> polyOpParser t

--------------------------------------------------------------------------------
-- COLOR Functions
--------------------------------------------------------------------------------
colorExprP :: Parser (SExpr SColor)
colorExprP =
  (ColorE <$> pColor)
    <|> betweenSquareBrackets
      ( do
          op <- betweenDoubleQuotes colorSymbol
          _ <- optional (char ',' >> space)
          colorOpParser op
      )

colorOpParser :: ColorToken -> Parser (SExpr SColor)
colorOpParser TRgba = undefined
colorOpParser TRgb = undefined
colorOpParser THsla = undefined
colorOpParser THsl = undefined
colorOpParser CInterpolate = do
  interType <- interpolationTypeP
  _ <- char ',' >> space
  input <- numExprP
  _ <- char ',' >> space
  InterpolateColorE interType input <$> inOutPairs `sepBy` (char ',' >> space)
  where
    inOutPairs = do
      num1 <- numExprP
      _ <- char ',' >> space
      color <- pColor
      return (num1, color)

--------------------------------------------------------------------------------
-- POLYMORPHIC Functions
--------------------------------------------------------------------------------
-- note that literals are checked separately, as they are not recognized otherwise
polyExprP :: Parser (SExpr SData)
polyExprP =
  try $
    choice
      [ FromNum . NumE <$> pNum
      , FromString . StringE <$> pString
      , FromBool . BoolE <$> pBool
      , FromColor . ColorE <$> pColor
      ]
      <|> betweenSquareBrackets
        ( do
            op <- betweenDoubleQuotes polySymbol
            _ <- optional (char ',' >> space)
            polyOpParser op
        )

polyOpParser :: PolyToken -> Parser (SExpr SData)
polyOpParser Get = FgetE <$> stringExprP
polyOpParser At = do
  lst <- ListE <$> pArray
  _ <- char ',' >> space
  AtE lst <$> numExprP
polyOpParser Match = do
  v <- polyExprP
  _ <- char ',' >> space
  cases' <- polyExprP `sepBy` (char ',' >> space)
  let (cases, fallback) = tuplifyWithFallback cases'
  return $ MatchE v cases fallback
  where
    tuplifyWithFallback :: [a] -> ([(a, a)], a)
    tuplifyWithFallback [] = error "no fallback value"
    tuplifyWithFallback [_] = error "no fallback value"
    tuplifyWithFallback (x : y : xs) = go [(x, y)] y xs
      where
        go pairs _ [] = (pairs, y) -- Return the pairs and the last element
        go pairs prev (z : zs) = go (pairs ++ [(prev, z)]) z zs
polyOpParser (PNum t) = FromNum <$> numOpParser t
polyOpParser (PString t) = FromString <$> stringOpParser t
polyOpParser (PBool t) = FromBool <$> boolOpParser t
polyOpParser (PColor t) = FromColor <$> colorOpParser t

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

interpolationTypeP :: Parser InterpolationType
interpolationTypeP = betweenSquareBrackets $ do
  try linear <|> try exponential <|> try cubicBezier
  where
    linear = do
      _ <- betweenDoubleQuotes $ string "linear"
      return Linear
    exponential = do
      _ <- betweenDoubleQuotes $ string "exponential"
      _ <- char ',' >> space
      Exponential <$> pNum
    cubicBezier = do
      _ <- betweenDoubleQuotes $ string "cubic-bezier"
      _ <- char ',' >> space
      x1 <- pNum
      _ <- char ',' >> space
      x2 <- pNum
      _ <- char ',' >> space
      y1 <- pNum
      _ <- char ',' >> space
      CubicBezier x1 x2 y1 <$> pNum

numOrStringP :: Parser NumOrString
numOrStringP = (Left <$> numExprP) <|> (Right <$> stringExprP)

pTraversable :: Parser STraversable
pTraversable = (Left <$> pArray) <|> (Right <$> stringExprP)
