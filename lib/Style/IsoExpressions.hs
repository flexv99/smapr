{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Style.IsoExpressions where

import Control.Lens
import Data.Bifunctor
import Data.Colour
import Data.Colour.RGBSpace
import Data.Colour.SRGB
import Data.List
import qualified Data.Map as MP
import Data.Maybe
import qualified Data.Text as T (findIndex)
import qualified Data.Text.Lazy as T
import Proto.Util
import Proto.Vector_tile.Tile.Value
import Style.ExpressionsContext
import Style.ExpressionsWrapper
import Style.Parser
import Text.Megaparsec
import Text.Megaparsec.Char

instance SParseable T.Text where
  sParse = stringExprP
  sEval = eval

instance SParseable INum where
  sParse = numExprP
  sEval = eval

instance SParseable Bool where
  sParse = boolExprP
  sEval = eval

instance SParseable Color where
  sParse = colorExprP
  sEval = eval

instance SParseable SType where
  sParse = stypeExprP
  sEval = eval

-- instance SParseable [a] where
--   sParse = arrayExprP
--   sEval = evalArrayExpr

--------------------------------------------------------------------------------

-- ISO Parsers

--------------------------------------------------------------------------------

-- >>> parseMaybe arithmethicExprP "[\"+\", 1, [\"/\", 1, 2]]"
arithmethicExprP :: Parser (IsoExpr INum)
arithmethicExprP =
  choice $
    map
      try
      [ AddE <$> exprBaseP "+" (numExprP `sepBy` (char ',' >> space)),
        exprBaseP "-" (SubE <$> argWithComma <*> numExprP),
        ProdE <$> exprBaseP "*" (numExprP `sepBy` (char ',' >> space)),
        exprBaseP "/" (DivE <$> argWithComma <*> numExprP)
      ]
  where
    argWithComma = do
      val <- numExprP
      _ <- char ',' >> space
      return val

-- >>> fmap evalExpr $ parseMaybe eqP "[\"==\", [1, 2, 3], [123]]"
-- false
-- >> evalExpr <$> parseMaybe eqP "[\"==\", [\"+\", 123, 4], 127]"
-- true
eqP :: Parser (IsoExpr Bool)
eqP = betweenSquareBrackets $ do
  key <- betweenDoubleQuotes (string "!=" <|> string "==")
  _ <- char ',' >> space
  arg1 <- polyExprP
  _ <- char ',' >> space
  arg2 <- cParserForType arg1
  let expr = EqE arg1 arg2
  if T.isPrefixOf "!" key then return $ Negation expr else return expr

negationP :: Parser (IsoExpr Bool)
negationP = betweenSquareBrackets $ do
  key <- betweenDoubleQuotes (char '!')
  _ <- char ',' >> space
  Negation <$> boolExprP

ordTypeP :: Parser OrdType
ordTypeP = choice $ map try [less, lessEq, greater, greaterEq]
  where
    construct s t = betweenDoubleQuotes $ do
      _ <- string s
      return t
    less = construct "<" Less
    lessEq = construct "<=" LessEq
    greater = construct ">" Greater
    greaterEq = construct ">=" GreaterEq

ordP :: Parser (IsoExpr Bool)
ordP = betweenSquareBrackets $ do
  let argsP = numExprP
  key <- ordTypeP
  _ <- char ',' >> space
  arg1 <- argsP
  _ <- char ',' >> space
  OrdE key arg1 <$> argsP

atP :: (SParseable a, Show a) => Parser (IsoExpr a) -> Parser (IsoExpr a)
atP elem = exprBaseP "at" $ do
  val1 <- betweenSquareBrackets $ do
    elem `sepBy` (char ',' >> space)
  _ <- char ',' >> space
  AtE val1 <$> numExprP

inP :: Parser (IsoExpr Bool)
inP = exprBaseP "in" $ do
  val1 <- pAtom
  _ <- char ',' >> space
  InE val1 <$> (try (LString <$> pString) <|> (LArray <$> pArray))

indexOfP :: Parser (IsoExpr INum)
indexOfP = exprBaseP "index-of" $ do
  val <- pAtom
  _ <- char ',' >> space
  arr <- (LString <$> pString) <|> LArray <$> pArray
  return $ IndexOfE val arr

lengthP :: Parser (IsoExpr INum)
lengthP = exprBaseP "length" $ do
  LengthE <$> ((LString <$> pString) <|> LArray <$> pArray)

allP :: Parser (IsoExpr Bool)
allP = betweenSquareBrackets $ do
  _ <- betweenDoubleQuotes $ string "all"
  _ <- char ',' >> space
  AllE <$> boolExprP `sepBy` (char ',' >> space)

matchP :: (Show a, SParseable a) => (SType -> a) -> Parser (IsoExpr a)
matchP unwrap = betweenSquareBrackets $ do
  _ <- betweenDoubleQuotes $ string "match"
  _ <- char ',' >> space
  expr <- polyExprP
  _ <- char ',' >> space
  args <- pAtom `sepBy` (char ',' >> space)
  let cases = bimap (map (second unwrap)) unwrap $ tuplifyWithFallback args
  return $ MatchE expr cases

caseP :: (Show a, SParseable a) => Parser (IsoExpr a)
caseP = betweenSquareBrackets $ do
  _ <- betweenDoubleQuotes $ string "case"
  _ <- char ',' >> space
  choices <- many choicesP
  CaseE choices <$> sParse
  where
    choicesP = do
      arg1 <- boolExprP
      _ <- char ',' >> space
      arg2 <- sParse
      _ <- char ',' >> space
      return (arg1, arg2)

coalesceP :: Parser (IsoExpr SType)
coalesceP = betweenSquareBrackets $ do
  _ <- betweenDoubleQuotes $ string "coalesce"
  _ <- char ',' >> space
  CoalesceE <$> (polyExprP `sepBy` (char ',' >> space))

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
      Exponential <$> numberLitINumP
    cubicBezier = do
      _ <- betweenDoubleQuotes $ string "cubic-bezier"
      _ <- char ',' >> space
      x1 <- numberLitINumP
      _ <- char ',' >> space
      x2 <- numberLitINumP
      _ <- char ',' >> space
      y1 <- numberLitINumP
      _ <- char ',' >> space
      CubicBezier x1 x2 y1 <$> numberLitINumP

interpolateNumP :: Parser (IsoExpr INum)
interpolateNumP = betweenSquareBrackets $ do
  _ <- betweenDoubleQuotes $ string "interpolate"
  _ <- char ',' >> space
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

interpolateColorP :: Parser (IsoExpr Color)
interpolateColorP = betweenSquareBrackets $ do
  _ <- betweenDoubleQuotes $ string "interpolate"
  _ <- char ',' >> space
  interType <- interpolationTypeP
  _ <- char ',' >> space
  input <- numExprP
  _ <- char ',' >> space
  InterpolateColorE interType input <$> inOutPairs `sepBy` (char ',' >> space)
  where
    inOutPairs = do
      num1 <- numExprP
      _ <- char ',' >> space
      num2 <- pColor
      return (num1, num2)

--------------------------------------------------------------------------------
-- Context dependant
--------------------------------------------------------------------------------

typeParser :: Parser T.Text
typeParser = label "type" $ betweenSquareBrackets $ betweenDoubleQuotes $ do
  string "geometry-type"

-- | choice of possible filtering types:
-- id, type, feature properties
filterByP :: Parser FilterBy
filterByP =
  choice
    [ FId <$> pInteger,
      try $ FTypeOf <$ typeParser,
      FProp <$> pString
    ]

getP :: (T.Text -> a) -> Parser a
getP c = betweenSquareBrackets $ do
  _ <- betweenDoubleQuotes $ string "get"
  _ <- char ',' >> space
  c <$> pString

sTGetP :: Parser (IsoExpr SType)
sTGetP = getP GetE

sGetP :: Parser (IsoExpr T.Text)
sGetP = getP SgetE

nGetP :: Parser (IsoExpr INum)
nGetP = getP NgetE

bGetP :: Parser (IsoExpr Bool)
bGetP = getP BgetE

cGetP :: Parser (IsoExpr Color)
cGetP = getP CgetE

hasP :: Parser (IsoExpr Bool)
hasP = betweenSquareBrackets $ do
  _ <- betweenDoubleQuotes $ string "has"
  _ <- char ',' >> space
  HasE <$> pString

fgeometryP :: Parser (IsoExpr T.Text)
fgeometryP = betweenSquareBrackets $ do
  FgeometryE <$ betweenDoubleQuotes (string "geometry-type")

fzoomP :: Parser (IsoExpr INum)
fzoomP = betweenSquareBrackets $ do
  FzoomE <$ betweenDoubleQuotes (string "zoom")

--------------------------------------------------------------------------------
-- Combined Parsers
--------------------------------------------------------------------------------

-- | 4all expressions that return string
stringP :: [Parser (IsoExpr T.Text)]
stringP =
  [ StringE <$> pString <* hidden space,
    fgeometryP,
    -- polymorphic
    atP stringExprP,
    -- matchP,
    -- interpolateP,
    sGetP
    -- fgeometryP
  ]

stringExprP :: Parser (IsoExpr T.Text)
stringExprP = choice $ map try stringP

stringFuncP :: Parser (IsoExpr T.Text)
stringFuncP = choice $ map try (tail stringP)

-- | 4all expressions that return num
numP :: [Parser (IsoExpr INum)]
numP =
  [ numLitP,
    arithmethicExprP,
    fzoomP,
    indexOfP,
    -- polymorphic
    atP numExprP,
    matchP unwrapNum,
    interpolateNumP,
    nGetP
  ]
  where
    numLitP = try (NumE . SDouble <$> pDouble <* hidden space) <|> NumE . SInt <$> pInteger <* hidden space

numExprP :: Parser (IsoExpr INum)
numExprP = choice $ map try numP

numFuncP :: Parser (IsoExpr INum)
numFuncP = choice $ map try (tail numP)

-- | 4all expressions that return bool
boolP :: [Parser (IsoExpr Bool)]
boolP =
  [ BoolE <$> pBool <* hidden space,
    negationP,
    ordP,
    eqP,
    inP,
    allP,
    hasP,
    -- polymorphic
    atP
      boolExprP,
    matchP
      unwrapBool,
    bGetP
  ]

boolExprP :: Parser (IsoExpr Bool)
boolExprP = choice $ map try boolP

boolFuncP :: Parser (IsoExpr Bool)
boolFuncP = choice $ map try (tail boolP)

-- | 4all expressions that return array
arrayP :: [Parser (IsoExpr [a])]
arrayP =
  []

-- IsoArg . ArrayE <$> pArray <* hidden space,
-- polymorphic
-- atP,
-- matchP,
-- interpolateP,
-- fgetP

arrayExprP :: Parser (IsoExpr [a])
arrayExprP = choice $ map try arrayP

arrayFuncP :: Parser (IsoExpr [a])
arrayFuncP = choice $ map try (tail arrayP)

-- | 4all expressions that return color
colorP :: [Parser (IsoExpr Color)]
colorP =
  [ ColorE <$> pColor <* hidden space,
    -- polymorphic
    atP colorExprP,
    matchP unwrapColor,
    interpolateColorP,
    cGetP
  ]

colorExprP :: Parser (IsoExpr Color)
colorExprP = choice $ map try colorP

colorFuncP :: Parser (IsoExpr Color)
colorFuncP = choice $ map try (tail colorP)

-- | 4all expressions that return stype
stypeP :: [Parser (IsoExpr SType)]
stypeP =
  [ atP
      stypeExprP,
    sTGetP,
    STypeE
      <$> pAtom
      <* hidden space
  ]

stypeExprP :: Parser (IsoExpr SType)
stypeExprP = choice $ map try stypeP

-- | 4all Polymorphic expressions
polyExprP :: Parser WrappedExpr
polyExprP =
  choice
    [ wrap <$> stypeExprP, -- in first for a reason has precedence
      wrap <$> stringExprP,
      wrap <$> numExprP,
      wrap <$> boolExprP,
      wrap <$> arrayExprP,
      wrap <$> colorExprP
    ]

polyFuncP :: Parser WrappedExpr
polyFuncP =
  choice
    [ wrap <$> stringFuncP,
      wrap <$> numFuncP,
      wrap <$> boolFuncP,
      wrap <$> arrayFuncP,
      wrap <$> colorFuncP
    ]

cParserForType :: WrappedExpr -> Parser WrappedExpr
cParserForType (StringExpr a) = wrap <$> stringExprP
cParserForType (NumExpr a) = wrap <$> numExprP
cParserForType (BoolExpr a) = wrap <$> boolExprP
cParserForType (ColorExpr a) = wrap <$> colorExprP
cParserForType (STypeExpr a) = wrap <$> stypeExprP
cParserForType (ArrayExpr a) = wrap <$> arrayExprP

--------------------------------------------------------------------------------

-- ISO expressions evaluators

--------------------------------------------------------------------------------

-- | +
sSum :: [INum] -> INum
sSum = foldr sAdd (SInt 0)
  where
    sAdd :: INum -> INum -> INum
    sAdd (SInt i) (SInt j) = SInt $ i + j
    sAdd (SInt i) (SDouble j) = SDouble $ fromIntegral i + j
    sAdd (SDouble i) (SInt j) = SDouble $ i + fromIntegral j
    sAdd (SDouble i) (SDouble j) = SDouble $ i + j

-- | *
sProd :: [INum] -> INum
sProd = foldr prod (SInt 1)
  where
    prod :: INum -> INum -> INum
    prod (SInt i) (SInt j) = SInt $ i * j
    prod (SInt i) (SDouble j) = SDouble $ fromIntegral i * j
    prod (SDouble i) (SInt j) = SDouble $ i * fromIntegral j
    prod (SDouble i) (SDouble j) = SDouble $ i * j

-- | -
sSub :: INum -> INum -> INum
sSub (SInt i) (SInt j) = SInt $ i - j
sSub (SInt i) (SDouble j) = SDouble $ fromIntegral i - j
sSub (SDouble i) (SInt j) = SDouble $ i - fromIntegral j
sSub (SDouble i) (SDouble j) = SDouble $ i - j

-- | :
sDiv :: INum -> INum -> INum
sDiv (SInt i) (SInt j) = SDouble $ fromIntegral i / fromIntegral j
sDiv (SInt i) (SDouble j) = SDouble $ fromIntegral i / j
sDiv (SDouble i) (SInt j) = SDouble $ i / fromIntegral j
sDiv (SDouble i) (SDouble j) = SDouble $ i / j

-- | == & /=
sEq :: (Eq a) => a -> a -> Bool
sEq i j = i == j

-- | < & <= & > & >=
sOrd :: OrdType -> INum -> INum -> Bool
sOrd = op
  where
    op Less = (<)
    op LessEq = (<=)
    op Greater = (>)
    op GreaterEq = (>=)

-- | all
sAll :: [Bool] -> Bool
sAll = and

-- | at
sAt :: [a] -> INum -> a
sAt a (SInt i) = a !! i
sAt a (SDouble d) = a !! floor d

-- | in
sIn :: SType -> LookupT -> Bool
sIn (SString s) (LString b) = s `T.isInfixOf` b
sIn _ (LString _) = error "string lookup can be made with string only"
sIn v (LArray t) = v `elem` t

-- | index-of
sIndexOf :: SType -> LookupT -> INum
sIndexOf (SString s) (LString b) = maybe (SInt (-1)) SInt (substringIndex s b)
  where
    substringIndex :: T.Text -> T.Text -> Maybe Int
    substringIndex needle haystack
      | T.null needle = Just 0 -- Treat empty needle as found at the beginning
      | otherwise = findIndex 0
      where
        findIndex i
          | i + T.length needle > T.length haystack = Nothing
          | T.take (T.length needle) (T.drop i haystack) == needle = Just (fromIntegral i)
          | otherwise = findIndex (i + 1)
sIndexOf _ (LString b) = error "string lookup can be made with string only"
sIndexOf e (LArray a) = maybe (SInt (-1)) SInt (e `elemIndex` a)

-- | length
slength :: LookupT -> INum
slength (LString s) = SInt $ fromIntegral $ T.length s
slength (LArray a) = SInt $ length a

-- | match
sMatch :: SType -> ([(SType, a)], a) -> a
sMatch t (matches, fallback) = fromMaybe fallback (listToMaybe $ isIn matches)
  where
    binary (SArray a, b) = if t `elem` a then Just b else Nothing
    binary (a, b) = if a == t then Just b else Nothing
    isIn = mapMaybe binary

-- | case
sCase :: [(Bool, a)] -> a -> a
sCase ((b, r) : xs) f = if b then r else sCase xs f
sCase [] f = f

-- | coalesce
sCoalesce :: [SType] -> SType
sCoalesce exp = maybe SNull nonNullOrFallback (unsnoc exp)
  where
    unsnoc = foldr (\x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing
    nonNullOrFallback (SNull : xs, f) = nonNullOrFallback (xs, f)
    nonNullOrFallback (x : xs, f) = x
    nonNullOrFallback ([], f) = f

-- | interpolate
-- maybe move from associated list to map?
-- https://cmears.id.au/articles/linear-interpolation.html
-- test: https://github.com/maplibre/maplibre-style-spec/blob/main/test/integration/expression/tests/interpolate/linear/test.json
sInterpolateNr :: InterpolationType -> INum -> [(INum, INum)] -> INum
sInterpolateNr t i pts =
  let (t', index) = interpolationFactor t i pts
      output = map snd pts
   in interpolateNr (output !! index) (output !! (index + 1)) t'

sInterpolateColor :: InterpolationType -> INum -> [(INum, Color)] -> Color
sInterpolateColor t i pts =
  let (t', index) = interpolationFactor t i pts
      output = map snd pts
   in interpolateColor (output !! index) (output !! (index + 1)) (numToDouble t')

findStopsLessThenOrEqualTo :: [INum] -> INum -> Int
findStopsLessThenOrEqualTo labels value = fromMaybe 0 (findIndex (<= value) labels)

interpolationFactor :: InterpolationType -> INum -> [(INum, a)] -> (INum, Int)
interpolationFactor t v pts = (pMatch t v, index)
  where
    pMatch Linear v' = exponentialInterpolation v' 1 (labels !! index) (labels !! (index + 1))
    pMatch (Exponential e) v' = exponentialInterpolation v' e (labels !! index) (labels !! (index + 1))
    pMatch _ _ = error "cubic bezier not yet supported"
    labels = map fst pts
    index = findStopsLessThenOrEqualTo labels v

exponentialInterpolation :: INum -> INum -> INum -> INum -> INum
exponentialInterpolation input base lower upper
  | difference == 0 = 0
  | base == 1 = progress / difference
  | otherwise = (base ** progress - 1) / (base ** difference - 1)
  where
    difference = upper - lower
    progress = input - lower

numTupleToDouble :: [(INum, a)] -> [(Double, a)]
numTupleToDouble ((a, b) : xs) = (numToDouble a, b) : numTupleToDouble xs
numTupleToDouble [] = []

interpolateNr :: INum -> INum -> INum -> INum
interpolateNr from to t = from + t * (to - from)

interpolateColor :: Color -> Color -> Double -> Color
interpolateColor from to t =
  let blendedColour = blend t from to
      RGB r g b = toSRGB (blend t (pureColor from) (pureColor to))
      alpha = alphaChannel blendedColour
   in sRGB r g b `withOpacity` alpha

sGet :: T.Text -> ExpressionContext -> SType
sGet key ctx = fromMaybe SNull (key `MP.lookup` featureProperties ctx)

sHas :: T.Text -> ExpressionContext -> Bool
sHas key ctx = key `MP.member` featureProperties ctx

-- defaults to linestring if geometry cannot be retrieved from feature
sGeometryType :: ExpressionContext -> T.Text
sGeometryType ctx = fromMaybe "LINESTRING" (geometryTypeToString (ctx ^. feature))

evalZoom :: ExpressionContext -> INum
evalZoom ctx = SDouble (ctx ^. ctxZoom)

--------------------------------------------------------------------------------
-- evaluators
--------------------------------------------------------------------------------

evalWrapped :: WrappedExpr -> ExpressionContext -> SType
evalWrapped (StringExpr s) ctx = SString $ T.toCaseFold $ eval s ctx
evalWrapped (NumExpr n) ctx = SNum $ eval n ctx
evalWrapped (BoolExpr b) ctx = SBool $ eval b ctx
evalWrapped (ArrayExpr a) ctx = undefined
evalWrapped (ColorExpr c) ctx = SColor $ eval c ctx
evalWrapped (STypeExpr s) ctx = eval s ctx

eval :: (SParseable a) => IsoExpr a -> ExpressionContext -> a
eval (BoolE b) _ = b
eval (Negation e) ctx = not $ eval e ctx
eval (EqE o t) ctx = sEq (evalWrapped o ctx) (evalWrapped t ctx)
eval (OrdE t a b) ctx = sOrd t (eval a ctx) (eval b ctx)
eval (InE v t) ctx = sIn v t
eval (AllE v) ctx = sAll (map (`sEval` ctx) v)
eval (NumE e) _ = e
eval (AddE a) ctx = sSum (map (`eval` ctx) a)
eval (ProdE a) ctx = sProd (map (`eval` ctx) a)
eval (SubE a b) ctx = sSub (eval a ctx) (eval b ctx)
eval (DivE a b) ctx = sDiv (eval a ctx) (eval b ctx)
eval (StringE s) _ = s
eval (ColorE c) _ = c
eval (AtE l i) ctx = sAt (map (`eval` ctx) l) (eval i ctx)
eval (MatchE m v) ctx = sMatch (evalWrapped m ctx) v
eval (CaseE c f) ctx = sCase (map (\(a, b) -> (eval a ctx, sEval b ctx)) c) (sEval f ctx)
eval (CoalesceE n) ctx = sCoalesce (map (`evalWrapped` ctx) n)
eval (InterpolateNumE t e a) ctx = sInterpolateNr t (eval e ctx) (map (\(a', b) -> (eval a' ctx, b)) a)
eval (InterpolateColorE t e a) ctx = sInterpolateColor t (eval e ctx) (map (\(a', b) -> (eval a' ctx, b)) a)
eval (IndexOfE e a) ctx = sIndexOf e a
eval (LengthE a) ctx = slength a
eval (GetE k) ctx = sGet k ctx
eval (SgetE k) ctx = unwrapString $ sGet k ctx
eval (NgetE k) ctx = unwrapNum $ sGet k ctx
eval (BgetE k) ctx = unwrapBool $ sGet k ctx
eval (CgetE k) ctx = unwrapColor $ sGet k ctx
eval (HasE k) ctx = sHas k ctx
eval FgeometryE ctx = sGeometryType ctx
eval FzoomE ctx = evalZoom ctx
eval (STypeE s) ctx = s
eval x ctx = eval x ctx

--------------------------------------------------------------------------------
-- Poison
--------------------------------------------------------------------------------

unwrapString :: SType -> T.Text
unwrapString (SString n) = n
unwrapString _ = error "Can only unwrap string"

unwrapNum :: SType -> INum
unwrapNum (SNum n) = n
unwrapNum _ = error "Can only unwrap num"

unwrapBool :: SType -> Bool
unwrapBool (SBool b) = b
unwrapBool _ = error "Can only unwrap bool"

unwrapColor :: SType -> Color
unwrapColor (SColor c) = c
unwrapColor _ = error "Can only unwrap colors"
