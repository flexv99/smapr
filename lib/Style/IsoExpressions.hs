{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Style.IsoExpressions where

import Data.List
import Data.Maybe
import qualified Data.Text.Lazy as T
import Style.ExpressionsWrapper
import Style.FeatureExpressions
import Style.Parser
import Text.Megaparsec
import Text.Megaparsec.Char

--------------------------------------------------------------------------------

-- ISO Parsers

--------------------------------------------------------------------------------

-- >>> parseMaybe arithmethicExprP "[\"+\", 1, [\"/\", 1, 2]]"
arithmethicExprP :: Parser (ArgType (SNum a))
arithmethicExprP =
  choice $
    map
      try
      [ IsoArg . AddE <$> exprBaseP "+" (numExprP `sepBy` (char ',' >> space)),
        IsoArg <$> exprBaseP "-" (SubE <$> argWithComma <*> numExprP),
        IsoArg . ProdE <$> exprBaseP "*" (numExprP `sepBy` (char ',' >> space)),
        IsoArg <$> exprBaseP "/" (DivE <$> argWithComma <*> numExprP)
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
eqP :: Parser (ArgType ('SBool b))
eqP = betweenSquareBrackets $ do
  key <- betweenDoubleQuotes (string "!=" <|> string "==")
  _ <- char ',' >> space
  arg1 <- polyExprP
  _ <- char ',' >> space
  arg2 <- polyExprP
  let expr = EqE arg1 arg2
  if T.isPrefixOf "!" key then return $ IsoArg $ Negation expr else return $ IsoArg expr

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

ordP :: Parser (ArgType ('SBool b))
ordP = betweenSquareBrackets $ do
  let argsP = numExprP
  key <- ordTypeP
  _ <- char ',' >> space
  arg1 <- argsP
  _ <- char ',' >> space
  IsoArg . OrdE key arg1 <$> argsP

atP :: Parser (ArgType a)
atP = exprBaseP "at" $ do
  val1 <- arrayLitP
  _ <- char ',' >> space
  IsoArg . AtE val1 <$> numExprP

inP :: Parser (ArgType ('SBool b))
inP = exprBaseP "in" $ do
  val <- (wrap <$> numExprP) <|> (wrap <$> stringExprP)
  _ <- char ',' >> space
  traversable <- (wrap <$> arrayExprP) <|> (wrap <$> stringExprP)
  return $ IsoArg $ InE val traversable

allP :: Parser (ArgType ('SBool a))
allP = betweenSquareBrackets $ do
  _ <- betweenDoubleQuotes $ string "all"
  _ <- char ',' >> space
  IsoArg . AllE <$> boolExprP `sepBy` (char ',' >> space)

matchP :: Parser (ArgType a)
matchP = betweenSquareBrackets $ do
  _ <- betweenDoubleQuotes $ string "match"
  _ <- char ',' >> space
  expr <- polyExprP
  _ <- char ',' >> space
  IsoArg . MatchE expr <$> matchArgsP
  where
    matchArgsP :: Parser MatchArg
    matchArgsP = do
      args <- pAtom `sepBy` (char ',' >> space)
      return $ MatchArg $ tuplifyWithFallback args

caseP :: Parser (ArgType a)
caseP = betweenSquareBrackets $ do
  _ <- betweenDoubleQuotes $ string "case"
  _ <- char ',' >> space
  choices <- many choicesP
  IsoArg . CaseE choices <$> pAtom
  where
    choicesP = do
      arg1 <- boolExprP
      _ <- char ',' >> space
      arg2 <- pAtom
      _ <- char ',' >> space
      return (arg1, arg2)

coalesceP :: Parser (ArgType a)
coalesceP = betweenSquareBrackets $ do
  _ <- betweenDoubleQuotes $ string "coalesce"
  _ <- char ',' >> space
  IsoArg . CoalesceE <$> (polyExprP `sepBy` (char ',' >> space))

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

interpolateP :: Parser (ArgType a)
interpolateP = betweenSquareBrackets $ do
  _ <- betweenDoubleQuotes $ string "interpolate"
  _ <- char ',' >> space
  interType <- interpolationTypeP
  _ <- char ',' >> space
  input <- numExprP
  _ <- char ',' >> space
  IsoArg . InterpolateE interType input <$> inOutPairs `sepBy` (char ',' >> space)
  where
    inOutPairs :: Parser (SType, WrappedExpr)
    inOutPairs = do
      num1 <- numberLitP
      _ <- char ',' >> space
      num2 <- (wrap <$> colorExprP) <|> (wrap <$> numExprP)
      return (num1, num2)

--------------------------------------------------------------------------------

-- ISO expressions evaluators

--------------------------------------------------------------------------------

-- | +
stypeSum :: [SType] -> SType
stypeSum = foldr stypeAdd (SNum $ SInt 0)
  where
    stypeAdd :: SType -> SType -> SType
    stypeAdd (SNum (SInt i)) (SNum (SInt j)) = SNum $ SInt $ i + j
    stypeAdd (SNum (SInt i)) (SNum (SDouble j)) = SNum $ SDouble $ fromIntegral i + j
    stypeAdd (SNum (SDouble i)) (SNum (SInt j)) = SNum $ SDouble $ i + fromIntegral j
    stypeAdd (SNum (SDouble i)) (SNum (SDouble j)) = SNum $ SDouble $ i + j
    stypeAdd _ _ = error "must be numeric type"

-- | *
stypeProd :: [SType] -> SType
stypeProd = foldr prod (SNum $ SInt 1)
  where
    prod :: SType -> SType -> SType
    prod (SNum (SInt i)) (SNum (SInt j)) = SNum $ SInt $ i * j
    prod (SNum (SInt i)) (SNum (SDouble j)) = SNum $ SDouble $ fromIntegral i * j
    prod (SNum (SDouble i)) (SNum (SInt j)) = SNum $ SDouble $ i * fromIntegral j
    prod (SNum (SDouble i)) (SNum (SDouble j)) = SNum $ SDouble $ i * j
    prod _ _ = error "must be numeric type"

-- | -
stypeSub :: SType -> SType -> SType
stypeSub (SNum (SInt i)) (SNum (SInt j)) = SNum $ SInt $ i - j
stypeSub (SNum (SInt i)) (SNum (SDouble j)) = SNum $ SDouble $ fromIntegral i - j
stypeSub (SNum (SDouble i)) (SNum (SInt j)) = SNum $ SDouble $ i - fromIntegral j
stypeSub (SNum (SDouble i)) (SNum (SDouble j)) = SNum $ SDouble $ i - j
stypeSub _ _ = error "must be numeric type"

-- | :
stypeDiv :: SType -> SType -> SType
stypeDiv (SNum (SInt i)) (SNum (SInt j)) = SNum $ SDouble $ fromIntegral i / fromIntegral j
stypeDiv (SNum (SInt i)) (SNum (SDouble j)) = SNum $ SDouble $ fromIntegral i / j
stypeDiv (SNum (SDouble i)) (SNum (SInt j)) = SNum $ SDouble $ i / fromIntegral j
stypeDiv (SNum (SDouble i)) (SNum (SDouble j)) = SNum $ SDouble $ i / j
stypeDiv _ _ = error "must be numeric type"

-- | == & /=
stypeEq :: SType -> SType -> SType
stypeEq (SNum i) (SNum j) = SBool $ i == j
stypeEq (SString i) (SString j) = SBool $ T.toCaseFold i == T.toCaseFold j
stypeEq (SBool i) (SBool j) = SBool $ i == j
stypeEq (SArray i) (SArray j) = SBool $ i == j
stypeEq _ _ = SBool False

-- | < & <= & > & >=
stypeOrd :: OrdType -> SType -> SType -> SType
stypeOrd o (SNum a) (SNum b) = SBool $ op o a b
  where
    op Less = (<)
    op LessEq = (<=)
    op Greater = (>)
    op GreaterEq = (>=)
stypeOrd _ _ _ = error "ord operator works on numbers only"

-- | at
stypeAt :: SType -> SType -> SType
stypeAt (SArray a) (SNum (SInt i)) = a !! i
stypeAt _ _ = error "args not matching"

-- | in
stypeIn :: SType -> SType -> SType
stypeIn (SString s) (SString t) = SBool $ s `T.isInfixOf` t
stypeIn v t = SBool $ v `elem` toList t
  where
    toList (SArray a) = a
    toList _ = error "can only unwrap an array"

-- | match
stypeMatch :: SType -> MatchArg -> SType
stypeMatch t (MatchArg (matches, fallback)) = fromMaybe fallback (listToMaybe $ isIn matches)
  where
    binary :: ToBeMatched -> Maybe SType
    binary (SArray a, b) = if t `elem` a then Just b else Nothing
    binary (a, b) = if a == t then Just b else Nothing
    isIn = mapMaybe binary

-- | case
stypeCase :: [(SType, SType)] -> SType -> SType
stypeCase ((SBool b, r) : xs) f = if b then r else stypeCase xs f
stypeCase [] f = f
stypeCase (_ : _) _ = error "condition must return bool"

stypeCoalesce :: [SType] -> SType
stypeCoalesce exp = maybe SNull nonNullOrFallback (unsnoc exp)
  where
    unsnoc = foldr (\x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing
    nonNullOrFallback (SNull : xs, f) = nonNullOrFallback (xs, f)
    nonNullOrFallback (x : xs, f) = x
    nonNullOrFallback ([], f) = f

-- | interpolate
-- maybe move from associated list to map?
-- https://cmears.id.au/articles/linear-interpolation.html
-- test: https://github.com/maplibre/maplibre-style-spec/blob/main/test/integration/expression/tests/interpolate/linear/test.json
stypeInterpolate :: InterpolationType -> SType -> [(SType, SType)] -> SType
stypeInterpolate t (SNum i) pts =
  let (t', index) = interpolationFactor t i pts
      output = map snd (numTupleToDouble pts)
   in SNum $ SDouble $ interpolateNr (output !! index) (output !! (index + 1)) t'
stypeInterpolate _ (SColor _) _ = undefined
stypeInterpolate _ _ _ = error "can only interpolate colors or numbers"

findStopsLessThenOrEqualTo :: [Double] -> Double -> Int
findStopsLessThenOrEqualTo labels value = fromMaybe 0 (findIndex (<= value) labels)

interpolationFactor :: InterpolationType -> INum -> [(SType, SType)] -> (Double, Int)
interpolationFactor t v pts = (pMatch t (numToDouble v), index)
  where
    pMatch Linear v' = exponentialInterpolation v' 1 (labels !! index) (labels !! (index + 1))
    pMatch (Exponential e) v' = exponentialInterpolation v' (numToDouble e) (labels !! index) (labels !! (index + 1))
    pMatch _ _ = error "cubic bezier not yet supported"
    toNums = numTupleToDouble pts
    labels = map fst toNums
    index = findStopsLessThenOrEqualTo labels (numToDouble v)

exponentialInterpolation :: (Eq a, Floating a) => a -> a -> a -> a -> a
exponentialInterpolation input base lower upper
  | difference == 0 = 0
  | base == 1 = progress / difference
  | otherwise = (base ** progress - 1) / (base ** difference - 1)
  where
    difference = upper - lower
    progress = input - lower

numTupleToDouble :: [(SType, SType)] -> [(Double, Double)]
numTupleToDouble ((SNum a, SNum b) : xs) = (numToDouble a, numToDouble b) : numTupleToDouble xs
numTupleToDouble ((_, _) : _) = error "tuples must be of numerical type"
numTupleToDouble [] = []

interpolateNr :: (Floating a) => a -> a -> a -> a
interpolateNr from to t = from + t * (to - from)

--------------------------------------------------------------------------------
-- Combined Parsers
--------------------------------------------------------------------------------

-- | 4all expressions that return string
stringP :: [Parser (ArgType (SString s))]
stringP =
  [ IsoArg . StringE <$> pString <* hidden space,
    -- polymorphic
    atP,
    matchP,
    interpolateP,
    fgetP,
    fgeometryP
  ]

stringExprP :: Parser (ArgType (SString s))
stringExprP = choice $ map try stringP

stringFuncP :: Parser (ArgType (SString s))
stringFuncP = choice $ map try (tail stringP)

-- | 4all expressions that return num
numP :: [Parser (ArgType (SNum i))]
numP =
  [ numLitP,
    arithmethicExprP,
    -- polymorphic
    atP,
    matchP,
    interpolateP,
    fgetP,
    fzoomP
  ]
  where
    numLitP = try (IsoArg . DoubleE <$> pDouble <* hidden space) <|> IsoArg . IntE <$> pInteger <* hidden space

numExprP :: Parser (ArgType (SNum i))
numExprP = choice $ map try numP

numFuncP :: Parser (ArgType (SNum i))
numFuncP = choice $ map try (tail numP)

-- | 4all expressions that return bool
boolP :: [Parser (ArgType (SBool b))]
boolP =
  [ IsoArg . BoolE <$> pBool <* hidden space,
    ordP,
    eqP,
    inP,
    allP,
    -- polymorphic
    atP,
    matchP,
    interpolateP,
    fgetP
  ]

boolExprP :: Parser (ArgType (SBool b))
boolExprP = choice $ map try boolP

boolFuncP :: Parser (ArgType (SBool b))
boolFuncP = choice $ map try (tail boolP)

-- | 4all expressions that return array
arrayP :: [Parser (ArgType (SArray a))]
arrayP =
  [ IsoArg . ArrayE <$> arrayLitP <* hidden space,
    -- polymorphic
    atP,
    matchP,
    interpolateP,
    fgetP
  ]

arrayExprP :: Parser (ArgType (SArray a))
arrayExprP = choice $ map try arrayP

arrayFuncP :: Parser (ArgType (SArray a))
arrayFuncP = choice $ map try (tail arrayP)

-- | 4all expressions that return color
colorP :: [Parser (ArgType (SColor c))]
colorP =
  [ IsoArg . ColorE <$> pColor <* hidden space,
    -- polymorphic
    atP,
    matchP,
    interpolateP,
    fgetP
  ]

colorExprP :: Parser (ArgType (SColor a))
colorExprP = choice $ map try colorP

colorFuncP :: Parser (ArgType (SColor a))
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
