{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE DataKinds      #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}

module Style.IsoExpressions where

import Data.Kind (Type)
import qualified Data.Text.Lazy as T
import qualified Text.Megaparsec.Char.Lexer as L
import Proto.Vector_tile.Tile.Feature (Feature(..))
import Proto.Vector_tile.Tile.Layer (Layer(..))
import Data.Maybe
import Data.List
import Text.Megaparsec
import Text.Megaparsec.Char
import Style.ExpressionsWrapper
import Style.FeatureExpressions
import Style.Parser

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

arrayExprP :: Parser (IsoExpr (SArray a))
arrayExprP = ArrayE <$> arrayLitP <* hidden space

litExprChoicheP :: Parser WrappedExpr
litExprChoicheP = choice [ wrap . IsoArg <$> intExprP
                         , wrap . IsoArg <$> doubleExprP
                         , wrap . IsoArg <$> boolExprP
                         , wrap . IsoArg <$> stringExprP
                         , wrap . IsoArg <$> arrayExprP
                         ]

-- >>> fmap evalExpr $ parseMaybe numRetExprP "[\"+\", 1, [\"/\", 1, 2]]"
-- 1.5
numRetExprP :: Parser (ArgType (SNum a))
numRetExprP = choice $ map try [ IsoArg <$> numExprP
                               , fzoomP
                               , IsoArg . AddE <$> exprBaseP "+"  (singleArgP  `sepBy` (char ',' >> space))
                               , IsoArg <$> exprBaseP "-" (SubE <$> argWithComma <*> singleArgP)
                               , IsoArg . ProdE <$> exprBaseP "*" (singleArgP `sepBy` (char ',' >> space))
                               , IsoArg <$> exprBaseP "/" (DivE <$> argWithComma <*> singleArgP)
                               ]
              where
                singleArgP = (wrap . IsoArg <$> numExprP) <|> (wrap <$> numRetExprP)
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

atP :: Parser (ArgType a)
atP = exprBaseP "at" $ do
  val1 <- arrayLitP
  _ <- char ',' >> space
  IsoArg . AtE val1 <$> intExprP

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
      tuplify :: [a] -> ([(a,a)], a)
      tuplify []       = error "no fallback value"
      tuplify [x]      = ([], x)
      tuplify (x:y:xs) =
        let (tuples, lastElem) = tuplify xs
        in ((x, y) : tuples, lastElem)
      matchArgsP :: Parser MatchArg
      matchArgsP = do
        args <- pAtom  `sepBy` (char ',' >> space)
        return $ MatchArg $ tuplify args

interpolationTypeP :: Parser InterpolationType
interpolationTypeP = betweenSquareBrackets $ do
    try linear <|> try exponential <|> try cubicBezier
      where
        linear      = do
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
      inOutPairs :: Parser (SType, ArgType (SNum n))
      inOutPairs = do
        num1 <- numberLitP
        _ <- char ',' >> space
        num2 <- numRetExprP <|> IsoArg <$> numExprP
        return (num1, num2)


--------------------------------------------------------------------------------

-- ISO expressions evaluators

--------------------------------------------------------------------------------

-- | +
stypeSum :: [SType] -> SType
stypeSum = foldr stypeAdd (SNum $ SInt 0)
  where
    stypeAdd :: SType -> SType -> SType
    stypeAdd (SNum (SInt i))    (SNum (SInt j))     = SNum $ SInt $ i + j
    stypeAdd (SNum (SInt i))    (SNum (SDouble j))  = SNum $ SDouble $ fromIntegral i + j
    stypeAdd (SNum (SDouble i)) (SNum (SInt j))     = SNum $ SDouble $ i + fromIntegral j
    stypeAdd (SNum (SDouble i)) (SNum (SDouble j))  = SNum $ SDouble $ i + j
    stypeAdd _ _                                    = error "must be numeric type"

-- | *
stypeProd :: [SType] -> SType
stypeProd = foldr stypeProd (SNum $ SInt 1)
  where
    stypeProd :: SType -> SType -> SType
    stypeProd (SNum (SInt i))    (SNum (SInt j))     = SNum $ SInt $ i * j
    stypeProd (SNum (SInt i))    (SNum (SDouble j))  = SNum $ SDouble $ fromIntegral i * j
    stypeProd (SNum (SDouble i)) (SNum (SInt j))     = SNum $ SDouble $ i * fromIntegral j
    stypeProd (SNum (SDouble i)) (SNum (SDouble j))  = SNum $ SDouble $ i * j
    stypeProd _ _                                    = error "must be numeric type"

-- | -
stypeSub :: SType -> SType -> SType
stypeSub (SNum (SInt i))    (SNum (SInt j))    = SNum $ SInt $ i - j
stypeSub (SNum (SInt i))    (SNum (SDouble j)) = SNum $ SDouble $ fromIntegral i - j
stypeSub (SNum (SDouble i)) (SNum (SInt j))    = SNum $ SDouble $ i - fromIntegral j
stypeSub (SNum (SDouble i)) (SNum (SDouble j)) = SNum $ SDouble $ i - j
stypeSub _ _                                   = error "must be numeric type"

-- | :
stypeDiv :: SType -> SType -> SType
stypeDiv (SNum (SInt i))    (SNum (SInt j))    = SNum $ SDouble $ fromIntegral i / fromIntegral j
stypeDiv (SNum (SInt i))    (SNum (SDouble j)) = SNum $ SDouble $ fromIntegral i / j
stypeDiv (SNum (SDouble i)) (SNum (SInt j))    = SNum $ SDouble $ i / fromIntegral j
stypeDiv (SNum (SDouble i)) (SNum (SDouble j)) = SNum $ SDouble $ i / j
stypeDiv _ _                                   = error "must be numeric type"

-- | == & /=
stypeEq :: SType -> SType -> SType
stypeEq (SNum i)    (SNum j)    = SBool $ i == j
stypeEq (SString i) (SString j) = SBool $ T.toCaseFold i == T.toCaseFold j
stypeEq (SBool i)   (SBool j)   = SBool $ i == j
stypeEq (SArray i)  (SArray j)  = SBool $ i == j
stypeEq _ _                     = error "eq on not supported types"

-- | in & !in
stypeIn :: SType -> SType -> SType
stypeIn (SArray a) (SNum (SInt i)) = a !! i
stypeIn _          (SNum (SInt i)) = error "param 1 must be an array"
stypeIn (SArray a) _               = error "param 2 must be an int"

-- | match
stypeMatch :: SType -> MatchArg -> SType
stypeMatch t (MatchArg (matches, fallback)) = fromMaybe fallback (listToMaybe $ isIn t matches)
  where
    isIn t = mapMaybe (\(a, b) -> if a == t then Just b else Nothing)

-- | interpolate
-- maybe move from associated list to map?
-- https://cmears.id.au/articles/linear-interpolation.html
-- test: https://github.com/maplibre/maplibre-style-spec/blob/main/test/integration/expression/tests/interpolate/linear/test.json
stypeInterpolate :: InterpolationType -> SType -> [(SType, SType)] -> SType
stypeInterpolate t (SNum i) pts   = let
  (t', index) = interpolationFactor t i pts
  output = map snd (numTupleToDouble pts)
  in SNum $ SDouble $ interpolateNr (output !! index) (output !! (index + 1)) t'
stypeInterpolate t (SColor c) pts = undefined
stypeInterpolate t _ pts          = error "can only interpolate colors or numbers"

findStopsLessThenOrEqualTo :: [Double] -> Double -> Int
findStopsLessThenOrEqualTo labels value = fromMaybe 0 (findIndex (<= value) labels)

interpolationFactor :: InterpolationType -> INum -> [(SType, SType)] -> (Double, Int)
interpolationFactor t v pts = (pMatch t (numToDouble v) pts, index)
  where
    pMatch  Linear v pts         = exponentialInterpolation v 1 (labels !! index) (labels !! (index + 1))
    pMatch (Exponential e) v pts = exponentialInterpolation v (numToDouble e) (labels !! index) (labels !! (index + 1))
    pMatch _               v pts = error "cubic bezier not yet supported"
    toNums  = numTupleToDouble pts
    labels  = map fst toNums
    index   = findStopsLessThenOrEqualTo labels (numToDouble v)

exponentialInterpolation :: (Eq a, Floating a) => a -> a -> a -> a -> a
exponentialInterpolation input base lower upper
  | difference == 0 = 0
  | base == 1 = progress / difference
  | otherwise = (base ** progress - 1) / (base ** difference - 1)
  where
      difference = upper - lower
      progress   = input - lower

numTupleToDouble :: [(SType, SType)] -> [(Double, Double)]
numTupleToDouble ((SNum a, SNum b):xs) = (numToDouble a, numToDouble b) : numTupleToDouble xs
numTupleToDouble ((_ , _):xs)          = error "tuples must be of numerical type"
numTupleToDouble []                    = []

interpolateNr :: Floating a => a -> a -> a -> a
interpolateNr from to t = from + t * (to - from)

