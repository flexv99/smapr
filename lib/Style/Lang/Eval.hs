{-# LANGUAGE MonoLocalBinds #-}

module Style.Lang.Eval (eval) where

import Control.Lens
import Control.Monad.Reader
import Data.Colour
import Data.List
import qualified Data.Map as MP
import Data.Maybe
import Data.Scientific
import qualified Data.Text.Lazy as T
import Proto.Util
import Style.ExpressionsContext
import Style.Lang.Ast
import Style.Lang.Parser
import Style.Lang.Types

eval :: SExpr a -> Reader ExpressionContext a
eval (NumE i) = return i
eval (NumCastE n) = unwrapN <$> eval n
  where
    unwrapN :: SData -> SNum
    unwrapN (DNum n) = n
    unwrapN _ = Nothing
eval (NumberE n) = multiOp (listToMaybe . mapMaybe numVal) n
  where
    numVal :: SData -> SNum
    numVal (DNum (Just x)) = Just x
    numVal _ = Nothing
eval (AddE a) = multiOp (fmap sum . sequence) a
eval (ProdE p) = multiOp (fmap product . sequence) p
eval (SubE s1 s2) = binaryOp (\a b -> (-) <$> a <*> b) s1 s2
eval (DivE s1 s2) = binaryOp (\a b -> (/) <$> a <*> b) s1 s2
eval (InterpolateNumE t e a) =
  eval e >>= \r ->
    fmap
      (fmap fromFloatDigits . (<*>) (sInterpolateNr t . toRealFloat <$> r))
      (revealTuple a)
  where
    revealTuple :: [(SExpr SNum, SNum)] -> Reader ExpressionContext (Maybe [(Double, Double)])
    revealTuple tuples = do
      results <- traverse processTuple tuples
      return $ sequence results
    processTuple :: (SExpr SNum, SNum) -> Reader ExpressionContext (Maybe (Double, Double))
    processTuple (f, s) = do
      maybeX <- eval f
      return $ mTuple (toRealFloat <$> maybeX, toRealFloat <$> s)
    mTuple :: (Maybe a, Maybe a) -> Maybe (a, a)
    mTuple (Just a, Just b) = Just (a, b)
    mTuple _ = Nothing
eval FzoomE = ask >>= \ctx -> return $ Just $ fromFloatDigits (ctx ^. ctxZoom)
eval (IndexOfListE o e) = monoOp (\e' -> handleNothing (fromIntegral <$> elemIndex o e')) e
  where
    handleNothing :: SNum -> SNum
    handleNothing (Just x) = Just x
    handleNothing Nothing = Just (-1)
eval (IndexOfStringE c s) =
  monoOp
    ( \s' ->
        handleNothing
          (fmap fromIntegral $ join $ substringIndex <$> c <*> s')
    )
    s
  where
    handleNothing :: SNum -> SNum
    handleNothing (Just x) = Just x
    handleNothing Nothing = Just (-1)
    substringIndex :: T.Text -> T.Text -> Maybe Int
    substringIndex needle haystack
      | T.null needle = Just 0 -- Treat empty needle as found at the beginning
      | otherwise = findIndex' 0
      where
        findIndex' i
          | i + T.length needle > T.length haystack = Nothing
          | T.take (T.length needle) (T.drop i haystack) == needle = Just (fromIntegral i)
          | otherwise = findIndex' (i + 1)
eval (LengthOfListE l) = monoOp (Just . fromIntegral . length) l
eval (LengthOfStringE s) = monoOp (\s' -> fromIntegral . T.length <$> s') s
eval (StringE s) = return s
eval (StringCastE s) = unwrapS <$> eval s
  where
    unwrapS :: SData -> SString
    unwrapS (DString s) = s
    unwrapS _ = Nothing
eval (AtE i (Left l)) = binaryOp atImpl l i
  where
    atImpl :: [SData] -> SNum -> SData
    atImpl xs (Just i) = (!!) xs (floor $ toRealFloat i)
    atImpl xs _ = error "index is null"
eval (AtE i (Right r)) = textAt <$> eval r <*> eval i
  where
    textAt :: SString -> SNum -> SData
    textAt t i = DString $ T.singleton <$> (T.index <$> t <*> (floor . toRealFloat <$> i))
eval FgeometryE = ask >>= \ctx -> return $ geometryTypeToString (ctx ^. feature)
eval (UpcaseE s) = monoOp (T.toUpper <$>) s
eval (DowncaseE s) = monoOp (T.toLower <$>) s
eval (ConcatE s1 s2) = binaryOp (<>) s1 s2
eval (BoolE b) = return b
eval (BoolCastE b) = unwrapBool <$> eval b
  where
    unwrapBool (DBool b) = b
    unwrapBool _ = Nothing
eval (Negation b) = monoOp (not <$>) b
eval (EqE a1 a2) = binaryOp sEq a1 a2
  where
    sEq :: SData -> SData -> SBool
    sEq (DString s1) (DString s2) =
      ((==) . T.toCaseFold <$> s1)
        <*> (T.toCaseFold <$> s2)
    sEq a1 a2 = Just $ a1 == a2
eval (OrdE t a1 a2) = case a1 of
  (Left n) -> binaryOp (sOrd t) n (unwrapLeft a2)
  (Right s) -> binaryOp (sOrd t) s (unwrapRight a2)
  where
    unwrapLeft (Left l) = l
    unwrapLeft _ = error "err: value is a string"
    unwrapRight (Right l) = l
    unwrapRight _ = error "err: value is a number"
eval (InE e t) = case t of
  (Left l) -> binaryOp (\e' l' -> Just $ e' `elem` l') e l
  (Right s) -> binaryOp (\e' s' -> T.isInfixOf <$> toString s' <*> e') s e
  where
    toString :: SData -> SString
    toString (DString s) = s
    toString _ = Nothing
eval (HasE s) = eval s >>= featureHas
  where
    featureHas :: SString -> Reader ExpressionContext SBool
    featureHas key = do
      ctx <- ask
      return (fmap (\x -> MP.member x (featureProperties'' ctx)) key)
eval (AllE b) = multiOp (fmap and . sequence) b
eval (FgetE k) = eval k >>= featureGet
  where
    featureGet :: SString -> Reader ExpressionContext SData
    featureGet key = do
      ctx <- ask
      let props = featureProperties'' ctx
      return $ fromMaybe (DNum Nothing) ((`MP.lookup` props) =<< key)
eval (MatchE i c f) = do
  t <- eval i
  matches <- mapM (fmap tuplify . revTuple) c
  fallback <- eval f
  return $ sMatch t matches fallback
  where
    revTuple t = sequence [eval $ fst t, eval $ snd t]
    tuplify (x : x' : _) = (x, x')
    tuplify _ = error "err: not a list of 2 elems, this should newer happen"
eval (CaseE cs f) = do
  cs' <- mapM reveal cs
  f' <- eval f
  return $ sCase cs' f'
  where
    reveal (f, s) = do
      f' <- eval f
      s' <- eval s
      return (f', s')
eval (StepE i cs) = do
  i' <- eval i
  cs' <- mapM reveal cs
  return $ sStep i' cs'
  where
    reveal (f, s) = do
      f' <- eval f
      return (f', s)
eval (SDataE d) = return d
eval (FromNum n) = monoOp DNum n
eval (FromString s) = monoOp DString s
eval (FromBool b) = monoOp DBool b
eval (FromColor c) = monoOp DColor c
eval (FromArray a) = monoOp DArray a
eval (ArrE l) = return l
eval (ColorE c) = return c
eval (InterpolateColorE t i pts) = do
  i' <- eval i
  revT <- revealTuple pts
  let res = (interpolationFactor t . toRealFloat <$> i') <*> revT
  let index = snd <$> res
  let output = map snd pts
  return $
    interpolateColor
      (at index output)
      (at (fmap (+ 1) index) output)
      (fromFloatDigits . fst <$> res)
  where
    at :: Maybe Int -> [SColor] -> SColor
    at index outputs = (outputs !!) =<< index
    revealTuple :: [(SExpr SNum, SColor)] -> Reader ExpressionContext (Maybe [(Double, SColor)])
    revealTuple tuples = do
      results <- traverse tupleStep tuples
      return $ sequence results
    tupleStep :: (SExpr SNum, SColor) -> Reader ExpressionContext (Maybe (Double, SColor))
    tupleStep (x, y) = do
      maybeX <- eval x
      return $ processTuple (maybeX, y)
    processTuple :: (SNum, SColor) -> Maybe (Double, SColor)
    processTuple (Just x, c) = Just (toRealFloat x, c)
    processTuple _ = Nothing
eval x = error ("not yet implemented" ++ show x)

--------------------------------------------------------------------------------

monoOp :: (t -> b) -> SExpr t -> Reader ExpressionContext b
monoOp f a = eval a >>= \x -> return $ f x

binaryOp :: (t -> t1 -> b) -> SExpr t -> SExpr t1 -> Reader ExpressionContext b
binaryOp f a b = eval a >>= \x -> eval b >>= \y -> return $ f x y

multiOp :: ([a] -> b) -> [SExpr a] -> Reader ExpressionContext b
multiOp f a = f `fmap` traverse eval a

--------------------------------------------------------------------------------

getEval :: T.Text -> Reader ExpressionContext SData
getEval k = ask >>= \ctx -> return (featureProperties'' ctx MP.! k)

-- | interpolate
-- maybe move from associated list to map?
-- https://cmears.id.au/articles/linear-interpolation.html
-- test: https://github.com/maplibre/maplibre-style-spec/blob/main/test/integration/expression/tests/interpolate/linear/test.json
sInterpolateNr :: (Num a, RealFloat a) => InterpolationType -> a -> [(a, a)] -> a
sInterpolateNr t i pts =
  let (t', index) = interpolationFactor t i pts
      output = map snd pts
   in interpolateNr (output !! index) (output !! (index + 1)) t'

-- sInterpolateColor :: (Num a, RealFloat a) => InterpolationType -> a -> [(a, SColor)] -> SColor
-- sInterpolateColor t i pts =
--   let (t', index) = interpolationFactor t i pts
--       output = map snd pts
--    in interpolateColor (output !! index) (output !! (index + 1)) (fromFloatDigits t')

findStopsLessThenOrEqualTo :: (Num a, Ord a) => [a] -> a -> Int
findStopsLessThenOrEqualTo labels value = fromMaybe 0 (findIndex (<= value) labels)

interpolationFactor :: (Num a, RealFloat a) => InterpolationType -> a -> [(a, b)] -> (a, Int)
interpolationFactor t v pts = (pMatch t v, index)
  where
    pMatch Linear v' = exponentialInterpolation v' 1 (labels !! index) (labels !! (index + 1))
    pMatch (Exponential e) v' = exponentialInterpolation v' (expo e) (labels !! index) (labels !! (index + 1))
      where
        expo (Just x) = toRealFloat x
        expo Nothing = error "not a number"
    pMatch _ _ = error "cubic bezier not yet supported"
    labels = map fst pts
    index = findStopsLessThenOrEqualTo labels v

exponentialInterpolation :: (RealFloat a) => a -> a -> a -> a -> a
exponentialInterpolation input base lower upper
  | difference == 0 = 0
  | base == 1 = progress / difference
  | otherwise = (base ** progress - 1) / (base ** difference - 1)
  where
    difference = upper - lower
    progress = input - lower

interpolateNr :: (Num a) => a -> a -> a -> a
interpolateNr from to t = from + t * (to - from)

interpolateColor :: SColor -> SColor -> SNum -> SColor
interpolateColor from to t = blend . toRealFloat <$> t <*> from <*> to

-- | < & <= & > & >=
sOrd :: (Ord a) => OrdType -> Maybe a -> Maybe a -> SBool
sOrd t x y = op t <$> x <*> y
  where
    op OLess = (<)
    op OLessEq = (<=)
    op OGreater = (>)
    op OGreaterEq = (>=)

sMatch :: SData -> [(SData, SData)] -> SData -> SData
sMatch t matches fallback = fromMaybe fallback (listToMaybe $ isIn matches)
  where
    binary (DArray a, b) = if t `elem` a then Just b else Nothing
    binary (a, b) = if a == t then Just b else Nothing
    isIn = mapMaybe binary

sCase :: [(SBool, SData)] -> SData -> SData
sCase ((Just b, r) : xs) f = if b then r else sCase xs f
sCase [] f = f
sCase _ f = f

sStep :: SNum -> [(SData, SNum)] -> SData
sStep n xs = maybe (fst $ last xs) fst (find (\(_, b) -> fromMaybe True (isSmaller n b)) xs)
  where
    isSmaller :: SNum -> SNum -> SBool
    isSmaller n m = (<=) <$> n <*> m

-- isSmaller n Nothing = True

-- Testing shite:
-- >>> join $ join $ fmap (\r -> runReader r <$> ctx) (eval <$> parseMaybe stringExprP "[\"get\", \"class\"]")
-- >>> fmap (\r -> runReader r <$> ctx) (eval <$> parseMaybe numExprP "[\"interpolate\",[\"exponential\", 2],2,1,2,3,6]")
