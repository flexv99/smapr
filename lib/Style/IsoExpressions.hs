{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Style.IsoExpressions where

import Control.Lens
import Control.Monad.Reader
import Data.Bifunctor
import Data.Colour
import Data.Colour.RGBSpace
import Data.Colour.SRGB
import Data.Either
import Data.List
import qualified Data.Map as MP
import Data.Maybe
import qualified Data.Text.Lazy as T
import Proto.Util
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
  key <- ordTypeP
  _ <- char ',' >> space
  arg1 <- p'
  _ <- char ',' >> space
  OrdE key arg1 <$> sndP' arg1
  where
    p' = Left <$> numExprP <|> Right <$> stringExprP
    sndP' x = if isLeft x then Left <$> numExprP else Right <$> stringExprP

atP :: (SParseable a, Show a) => Parser (IsoExpr a)
atP = exprBaseP "at" $ do
  val1 <- betweenSquareBrackets $ do
    sParse `sepBy` (char ',' >> space)
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
allP = exprBaseP "all" $ do
  AllE <$> boolExprP `sepBy` (char ',' >> space)

matchP :: (Show a, SParseable a) => (SType -> a) -> Parser (IsoExpr a)
matchP unwrap = exprBaseP "match" $ do
  expr <- polyExprP
  _ <- char ',' >> space
  args <- pAtom `sepBy` (char ',' >> space)
  let cases = bimap (map (second unwrap)) unwrap $ tuplifyWithFallback args
  return $ MatchE expr cases

caseP :: (Show a, SParseable a) => Parser (IsoExpr a)
caseP = exprBaseP "case" $ do
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
coalesceP = exprBaseP "coalesce" $ do
  CoalesceE <$> (polyExprP `sepBy` (char ',' >> space))

stepP :: (Show a, SParseable a) => Parser a -> Parser (IsoExpr a)
stepP argP = exprBaseP "step" $ do
  step <- numExprP
  _ <- char ',' >> space
  StepE step <$> pairsP `sepBy` (char ',' >> space)
  where
    pairsP = do
      v <- argP
      stop <- optional (char ',' >> space >> pNum)
      return (v, stop)

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

stringAssertP :: Parser (IsoExpr T.Text)
stringAssertP = betweenSquareBrackets $ do
  _ <- betweenDoubleQuotes $ string "string"
  _ <- char ',' >> space
  StringAssertE <$> (polyFuncP `sepBy` (char ',' >> space))

numAssertP :: Parser (IsoExpr INum)
numAssertP = betweenSquareBrackets $ do
  _ <- betweenDoubleQuotes $ string "number"
  _ <- char ',' >> space
  NumberAssertE <$> (polyFuncP `sepBy` (char ',' >> space))

boolAssertP :: Parser (IsoExpr Bool)
boolAssertP = betweenSquareBrackets $ do
  _ <- betweenDoubleQuotes $ string "boolean"
  _ <- char ',' >> space
  BoolAssertE <$> (polyFuncP `sepBy` (char ',' >> space))

--------------------------------------------------------------------------------
-- Combined Parsers
--------------------------------------------------------------------------------

-- | 4all expressions that return string
stringP :: [Parser (IsoExpr T.Text)]
stringP =
  [ StringE <$> pString <* hidden space,
    stringAssertP,
    fgeometryP,
    -- polymorphic
    atP,
    matchP unwrapString,
    sGetP
  ]

stringExprP :: Parser (IsoExpr T.Text)
stringExprP = choice $ map try stringP

stringFuncP :: Parser (IsoExpr T.Text)
stringFuncP = choice $ map try (tail stringP)

-- | 4all expressions that return num
numP :: [Parser (IsoExpr INum)]
numP =
  [ numLitP,
    numAssertP,
    arithmethicExprP,
    fzoomP,
    indexOfP,
    -- polymorphic
    atP,
    matchP unwrapNum,
    interpolateNumP,
    nGetP,
    stepP pNum
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
    boolAssertP,
    negationP,
    ordP,
    eqP,
    inP,
    allP,
    hasP,
    -- polymorphic
    atP,
    matchP
      unwrapBool,
    bGetP,
    stepP pBool
  ]

boolExprP :: Parser (IsoExpr Bool)
boolExprP = choice $ map try boolP

boolFuncP :: Parser (IsoExpr Bool)
boolFuncP = choice $ map try (tail boolP)

-- | 4all expressions that return array
arrayP :: [Parser (IsoExpr [a])]
arrayP =
  []

arrayExprP :: Parser (IsoExpr [a])
arrayExprP = choice $ map try arrayP

arrayFuncP :: Parser (IsoExpr [a])
arrayFuncP = choice $ map try (tail arrayP)

-- | 4all expressions that return color
colorP :: [Parser (IsoExpr Color)]
colorP =
  [ ColorE <$> pColor <* hidden space,
    -- polymorphic
    atP,
    matchP unwrapColor,
    interpolateColorP,
    cGetP,
    stepP pColor
  ]

colorExprP :: Parser (IsoExpr Color)
colorExprP = choice $ map try colorP

colorFuncP :: Parser (IsoExpr Color)
colorFuncP = choice $ map try (tail colorP)

-- | 4all Polymorphic expressions
polyExprP :: Parser WrappedExpr
polyExprP =
  choice
    [ wrap <$> stringExprP,
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

-- polyOnlyP :: [Parser (IsoExpr a)]
-- polyOnlyP =
--   [ matchP,
--     caseP,
--     atP,
--     stepP
--   ]

cParserForType :: WrappedExpr -> Parser WrappedExpr
cParserForType (StringExpr a) = wrap <$> stringExprP
cParserForType (NumExpr a) = wrap <$> numExprP
cParserForType (BoolExpr a) = wrap <$> boolExprP
cParserForType (ColorExpr a) = wrap <$> colorExprP
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
sOrd :: (Ord a) => OrdType -> a -> a -> Bool
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

sGet :: T.Text -> Reader ExpressionContext SType
sGet key = ask >>= \ctx -> return $ fromMaybe SNull (key `MP.lookup` featureProperties ctx)

sHas :: T.Text -> Reader ExpressionContext Bool
sHas key = ask >>= \ctx -> return $ key `MP.member` featureProperties ctx

sStep :: INum -> [(a, Maybe INum)] -> a
sStep n xs = maybe (fst $ last xs) fst (find (\(_, b) -> isSmaller n b) xs)
  where
    isSmaller :: INum -> Maybe INum -> Bool
    isSmaller n (Just x) = n <= x
    isSmaller n Nothing = True

-- defaults to linestring if geometry cannot be retrieved from feature
sGeometryType :: Reader ExpressionContext T.Text
sGeometryType = ask >>= \ctx -> return $ fromMaybe "LINESTRING" (geometryTypeToString (ctx ^. feature))

evalZoom :: Reader ExpressionContext INum
evalZoom = ask >>= \ctx -> return $ SDouble (ctx ^. ctxZoom)

--------------------------------------------------------------------------------
-- evaluators
--------------------------------------------------------------------------------

evalWrapped :: WrappedExpr -> Reader ExpressionContext SType
evalWrapped (StringExpr s) = liftM (SString . T.toCaseFold) (eval s)
evalWrapped (NumExpr n) = liftM SNum $ eval n
evalWrapped (BoolExpr b) = liftM SBool $ eval b
evalWrapped (ArrayExpr a) = undefined
evalWrapped (ColorExpr c) = liftM SColor $ eval c

eval :: (SParseable a) => IsoExpr a -> Reader ExpressionContext a
eval (BoolE b) = return b
eval (Negation e) = eval e >>= \x -> return $ not x
eval (EqE o t) = liftM2 sEq (evalWrapped o) (evalWrapped t)
-- eval (OrdE t a b) = binaryOp (sOrd t) a b
eval (InE v t) = return $ sIn v t
eval (AllE v) = multiOp sAll v
eval (NumE e) = return e
eval (AddE a) = multiOp sSum a
eval (ProdE a) = multiOp sProd a
eval (SubE a b) = binaryOp sSub a b
eval (DivE a b) = binaryOp sDiv a b
eval (StringE s) = return s
eval (ColorE c) = return c
eval (AtE l i) = liftM2 sAt (traverse eval l) (eval i)
eval (MatchE m v) = liftM (`sMatch` v) (evalWrapped m)
eval (CaseE c f) = liftM2 sCase (traverse evalTuple c) (eval f)
  where
    evalTuple t = do
      t1 <- eval (fst t)
      t2 <- eval (snd t)
      return (t1, t2)
eval (CoalesceE n) = liftM sCoalesce (mapM evalWrapped n)
eval (InterpolateNumE t e a) = liftM2 (sInterpolateNr t) (eval e) (traverse revealTuple a)
  where
    revealTuple t = do
      t1 <- eval (fst t)
      return (t1, snd t)
eval (InterpolateColorE t e a) = liftM2 (sInterpolateColor t) (eval e) (traverse revealTuple a)
  where
    revealTuple t = do
      t1 <- eval (fst t)
      return (t1, snd t)
eval (IndexOfE e a) = return $ sIndexOf e a
eval (LengthE a) = return $ slength a
eval (SgetE k) = sGet k >>= return . unwrapString
eval (NgetE k) = sGet k >>= return . unwrapNum
eval (BgetE k) = sGet k >>= return . unwrapBool
eval (CgetE k) = sGet k >>= return . unwrapColor
eval (HasE k) = sHas k
eval (StepE f s) = liftM (`sStep` s) (eval f)
eval FgeometryE = sGeometryType
eval FzoomE = evalZoom
eval _ = error "exhausted eval"

multiOp :: (SParseable a) => ([a] -> b) -> [IsoExpr a] -> ReaderT ExpressionContext Identity b
multiOp f a = f `liftM` traverse (eval >>= \x -> return x) a

binaryOp :: (SParseable t, SParseable t1) => (t -> t1 -> b) -> IsoExpr t -> IsoExpr t1 -> ReaderT ExpressionContext Identity b
binaryOp f a b = eval a >>= \x -> eval b >>= \y -> return $ f x y

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
