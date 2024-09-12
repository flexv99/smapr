{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}

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

stringExprP :: Parser (IsoExpr (SString s))
stringExprP = StringE <$> pString <* hidden space

intExprP :: Parser (IsoExpr (SNum (SInt i)))
intExprP = IntE <$> pInteger <* hidden space

doubleExprP :: Parser (IsoExpr (SNum (SDouble d)))
doubleExprP = DoubleE <$> pDouble <* hidden space

numExprP :: Parser (IsoExpr (SNum a))
numExprP = NumE <$> (try doubleLitP <|> intLitP)

boolExprP :: Parser (IsoExpr (SBool b))
boolExprP = BoolE <$> pBool <* hidden space

colorExprP :: Parser (IsoExpr (SColor c))
colorExprP = ColorE <$> pColor <* hidden space

arrayExprP :: Parser (IsoExpr (SArray a))
arrayExprP = ArrayE <$> arrayLitP <* hidden space

litExprChoicheP :: Parser WrappedExpr
litExprChoicheP =
  choice
    [ wrap . IsoArg <$> intExprP,
      wrap . IsoArg <$> doubleExprP,
      wrap . IsoArg <$> boolExprP,
      wrap . IsoArg <$> stringExprP,
      wrap . IsoArg <$> arrayExprP
    ]

-- >>> fmap evalExpr $ parseMaybe numRetExprP "[\"+\", 1, [\"/\", 1, 2]]"
-- 1.5
numRetExprP :: Parser (ArgType (SNum a))
numRetExprP =
  choice $
    map
      try
      [ IsoArg <$> numExprP,
        fzoomP, -- todo Move to a combined one isolate iso and feature ones
        IsoArg . AddE <$> exprBaseP "+" (singleArgP `sepBy` (char ',' >> space)),
        IsoArg <$> exprBaseP "-" (SubE <$> argWithComma <*> singleArgP),
        IsoArg . ProdE <$> exprBaseP "*" (singleArgP `sepBy` (char ',' >> space)),
        IsoArg <$> exprBaseP "/" (DivE <$> argWithComma <*> singleArgP)
      ]
  where
    singleArgP = (IsoArg <$> numExprP) <|> numRetExprP
    argWithComma = do
      val <- singleArgP
      _ <- char ',' >> space
      return val

-- >>> fmap evalExpr $ parseMaybe eqP "[\"==\", [1, 2, 3], [123]]"
-- false
-- >> evalExpr <$> parseMaybe eqP "[\"==\", [\"+\", 123, 4], 127]"
-- true
eqP :: Parser (ArgType ('SBool b))
eqP = betweenSquareBrackets $ do
  let argsP = try (wrap <$> numRetExprP) <|> try (wrap <$> fgeometryP) <|> try (wrap <$> fgetP) <|> try litExprChoicheP
  key <- betweenDoubleQuotes (string "!=" <|> string "==")
  _ <- char ',' >> space
  arg1 <- argsP
  _ <- char ',' >> space
  arg2 <- argsP
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
  let argsP = numRetExprP
  key <- ordTypeP
  _ <- char ',' >> space
  arg1 <- argsP
  _ <- char ',' >> space
  IsoArg . OrdE key arg1 <$> argsP

atP :: Parser (ArgType a)
atP = exprBaseP "at" $ do
  val1 <- arrayLitP
  _ <- char ',' >> space
  IsoArg . AtE val1 . IsoArg <$> intExprP

inP :: Parser (ArgType ('SBool b))
inP = exprBaseP "in" $ do
  val <- (wrap . IsoArg <$> numExprP) <|> (wrap . IsoArg <$> stringExprP)
  _ <- char ',' >> space
  traversable <- (wrap . IsoArg <$> arrayExprP) <|> (wrap . IsoArg <$> stringExprP)
  return $ IsoArg $ InE val traversable

allP :: Parser (ArgType ('SBool a))
allP = betweenSquareBrackets $ do
  _ <- betweenDoubleQuotes $ string "all"
  _ <- char ',' >> space
  IsoArg . AllE <$> (try (IsoArg <$> boolExprP) <|> try eqP <|> try matchP) `sepBy` (char ',' >> space)

matchP :: Parser (ArgType a)
matchP = betweenSquareBrackets $ do
  _ <- betweenDoubleQuotes $ string "match"
  _ <- char ',' >> space
  expr <- wrap <$> fgetP
  _ <- char ',' >> space
  IsoArg . MatchE expr <$> matchArgsP
  where
    tuplify :: [a] -> ([(a, a)], a)
    tuplify [] = error "no fallback value"
    tuplify [x] = ([], x)
    tuplify (x : y : xs) =
      let (tuples, lastElem) = tuplify xs
       in ((x, y) : tuples, lastElem)
    matchArgsP :: Parser MatchArg
    matchArgsP = do
      args <- pAtom `sepBy` (char ',' >> space)
      return $ MatchArg $ tuplify args

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

interpolateP :: Parser (ArgType ('SNum n))
interpolateP = betweenSquareBrackets $ do
  _ <- betweenDoubleQuotes $ string "interpolate"
  _ <- char ',' >> space
  interType <- interpolationTypeP
  _ <- char ',' >> space
  input <- numRetExprP
  _ <- char ',' >> space
  IsoArg . InterpolateE interType input <$> inOutPairs `sepBy` (char ',' >> space)
  where
    inOutPairs :: Parser (SType, WrappedExpr)
    inOutPairs = do
      num1 <- numberLitP
      _ <- char ',' >> space
      num2 <- (wrap . IsoArg <$> colorExprP) <|> (wrap <$> numRetExprP) <|> (wrap . IsoArg <$> numExprP)
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
