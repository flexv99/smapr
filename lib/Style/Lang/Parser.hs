{-# LANGUAGE MonoLocalBinds #-}

module Style.Lang.Parser
  ( numExprP,
    stringExprP,
    boolExprP,
    colorExprP,
    arrayExprP,
    polyExprP,
  )
where

import Control.Monad (join)
import qualified Data.Aeson as A
import qualified Data.Text.Lazy as T
import Data.Vector (toList)
import Style.Lang.Ast
import Style.Lang.Lex
import Style.Lang.Token
import Style.Lang.Types
import Style.Lang.Util
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

instance A.FromJSON SData where
  parseJSON (A.Number n) = pure $ DNum $ Just n
  parseJSON (A.Bool b) = pure $ DBool $ Just b
  parseJSON (A.String s) = pure $ DString $ Just $ T.fromStrict s
  parseJSON (A.Array a) = DArray <$> traverse A.parseJSON (toList a)
  parseJSON A.Null = pure $ DString Nothing
  parseJSON (A.Object _) = pure $ DString Nothing
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
      num2 <- numExprP
      return (num1, num2)
numOpParser Zoom = return FzoomE
numOpParser IndexOf = do
  elem <- pAtom
  _ <- char ',' >> space
  elems <- traversableP
  case elems of
    (Left l) -> return $ IndexOfListE elem l
    (Right s) -> return $ IndexOfStringE (unwrapText elem) s
  where
    unwrapText :: SData -> SString
    unwrapText (DString s) = s
    unwrapText _ = Nothing -- for completeness, will newer happen
numOpParser Length = do
  elems <- traversableP
  case elems of
    (Left l) -> return $ LengthOfListE l
    (Right s) -> return $ LengthOfStringE s
numOpParser Number = NumberE <$> (try polyExprP `sepBy1` (char ',' >> space))
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
stringOpParser Upcase = UpcaseE <$> stringExprP
stringOpParser Downcase = DowncaseE <$> stringExprP
stringOpParser Concat = do
  s1 <- stringExprP
  _ <- char ',' >> space
  ConcatE s1 <$> stringExprP
stringOpParser TypeOf = do
  TypeOfE <$> polyExprP
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
boolOpParser Neg = Negation <$> boolExprP
boolOpParser Equality = do
  let argsP = SDataE <$> pAtom <|> polyExprP
  v1 <- argsP
  _ <- char ',' >> space
  EqE v1 <$> argsP
boolOpParser Less =
  OrdE OLess
    <$> numOrStringP
    <* (char ',' >> space)
    <*> numOrStringP
boolOpParser LessEq =
  OrdE OLessEq
    <$> numOrStringP
    <* (char ',' >> space)
    <*> numOrStringP
boolOpParser Greater =
  OrdE OGreater
    <$> numOrStringP
    <* (char ',' >> space)
    <*> numOrStringP
boolOpParser GreaterEq =
  OrdE OGreaterEq
    <$> numOrStringP
    <* (char ',' >> space)
    <*> numOrStringP
boolOpParser In =
  InE
    <$> polyExprP
    <* (char ',' >> space)
    <*> traversableP
boolOpParser All =
  AllE
    <$> boolExprP `sepBy` (char ',' >> space)
boolOpParser Any =
  AnyE <$> boolExprP `sepBy` (char ',' >> space)
boolOpParser Has = HasE <$> stringExprP
boolOpParser Boolean =
  BooleanE
    <$> (try polyExprP `sepBy1` (char ',' >> space))
boolOpParser (BPoly t) = BoolCastE <$> polyOpParser t

--------------------------------------------------------------------------------
-- ARRAY Functions
--------------------------------------------------------------------------------

arrayExprP :: Parser (SExpr [SData])
arrayExprP =
  betweenSquareBrackets
    ( do
        op <- betweenDoubleQuotes arraySymbol
        _ <- optional (char ',' >> space)
        arrayOpParser op
    )
    <|> try (ArrE <$> pArray)

arrayOpParser :: ArrayToken -> Parser (SExpr [SData])
arrayOpParser Array = ArrayE <$> (polyExprP `sepBy1` (char ',' >> space))
arrayOpParser (APoly a) = ArrayCastE <$> polyOpParser a

--------------------------------------------------------------------------------
-- COLOR Functions
--------------------------------------------------------------------------------
colorExprP :: Parser (SExpr SColor)
colorExprP =
  try (ColorE <$> pColor)
    <|> betweenSquareBrackets
      ( do
          op <- betweenDoubleQuotes (colorSymbol <|> CPoly <$> polySymbol)
          _ <- optional (char ',' >> space)
          colorOpParser op
      )

colorOpParser :: ColorToken -> Parser (SExpr SColor)
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
      color <- colorExprP
      return (num1, color)
colorOpParser (CPoly c) = ColorCastE <$> polyOpParser c
colorOpParser t = ColorE . Just <$> pFColor t -- actually this should newer get called

--------------------------------------------------------------------------------
-- POLYMORPHIC Functions
--------------------------------------------------------------------------------
-- note that literals are checked separately, as they are not recognized otherwise
polyExprP :: Parser (SExpr SData)
polyExprP =
  try $
    choice
      [ FromNum . NumE <$> pNum,
        FromBool . BoolE <$> pBool,
        try $ FromColor . ColorE <$> pColor,
        FromString . StringE <$> pString
      ]
      <|> betweenSquareBrackets
        ( do
            op <- betweenDoubleQuotes polySymbol
            _ <- optional (char ',' >> space)
            polyOpParser op
        )
      <|> (FromArray . ArrE <$> pArray) -- last chance

polyOpParser :: PolyToken -> Parser (SExpr SData)
polyOpParser Get = FgetE <$> stringExprP
polyOpParser At = do
  idx <- numExprP
  _ <- char ',' >> space
  AtE idx <$> traversableP
polyOpParser Match = do
  v <- polyExprP
  _ <- char ',' >> space
  cases' <- caseP `sepBy` (char ',' >> space)
  let (cases, fallback) = tuplify cases'
  return $ MatchE v cases fallback
  where
    caseP = polyExprP <|> (FromArray . ArrE <$> pArray)
    tuplify :: [a] -> ([(a, a)], a)
    tuplify xs
      | odd (length xs) = (pairUp initXs, last xs)
      | otherwise = error "List must have an odd number of elements"
      where
        initXs = init xs
        pairUp (x : y : rest) = (x, y) : pairUp rest
        pairUp _ = []
polyOpParser Case = do
  choices <- many choicesP
  CaseE choices <$> polyExprP
  where
    choicesP = do
      arg1 <- boolExprP
      _ <- char ',' >> space
      arg2 <- polyExprP
      _ <- char ',' >> space
      return (arg1, arg2)
polyOpParser Step = do
  step <- numExprP
  _ <- char ',' >> space
  StepE step <$> pairsP `sepBy` (char ',' >> space)
  where
    pairsP :: Parser (SExpr SData, SNum)
    pairsP = do
      v <- polyExprP
      stop <- optional (char ',' >> space >> pNum)
      return (v, join stop)
polyOpParser (PNum t) = FromNum <$> numOpParser t
polyOpParser (PArray t) = FromArray <$> arrayOpParser t
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
      num <- optional . try $ do
        _ <- char ',' >> space
        pNum
      return $ Linear (join num)
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

traversableP :: Parser STraversable
traversableP = (Left <$> arrayExprP) <|> (Right <$> stringExprP)
