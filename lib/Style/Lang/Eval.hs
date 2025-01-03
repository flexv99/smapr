{-# LANGUAGE MonoLocalBinds #-}

module Style.Lang.Eval (eval) where

import Control.Lens
import Control.Monad.Reader
import Data.Colour
import Data.Functor
import Data.Functor.Identity
import Data.List
import qualified Data.Map as MP
import Data.Maybe
import Data.Scientific
import qualified Data.Text.Lazy as T
import GHC.Float
import Proto.Util
import Style.ExpressionsContext
import Style.Lang.Ast
import Style.Lang.Parser
import Style.Lang.Types
import Style.Lang.Util

eval :: SExpr a -> Reader ExpressionContext a
eval (NumE i) = return i
eval (NumCastE n) = unwrapN <$> eval n
  where
    unwrapN :: SData -> SNum
    unwrapN (DNum n) = n
    unwrapN _ = Nothing
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
eval (IndexOfList o e) = return $ handleNothing (fromIntegral <$> elemIndex o e)
  where
    handleNothing :: SNum -> SNum
    handleNothing (Just x) = Just x
    handleNothing Nothing = Just (-1)
eval (IndexOfString c s) = return $ handleNothing (fmap fromIntegral $ join $ substringIndex <$> c <*> s)
  where
    handleNothing :: SNum -> SNum
    handleNothing (Just x) = Just x
    handleNothing Nothing = Just (-1)
    substringIndex :: T.Text -> T.Text -> Maybe Int
    substringIndex needle haystack
      | T.null needle = Just 0 -- Treat empty needle as found at the beginning
      | otherwise = findIndex 0
      where
        findIndex i
          | i + T.length needle > T.length haystack = Nothing
          | T.take (T.length needle) (T.drop i haystack) == needle = Just (fromIntegral i)
          | otherwise = findIndex (i + 1)
eval (StringE s) = return s
eval (StringCastE s) = unwrapS <$> eval s
  where
    unwrapS :: SData -> SString
    unwrapS (DString s) = s
    unwrapS _ = Nothing
eval (TextAtE t i) = textAt <$> eval t <*> eval i
  where
    textAt :: SString -> SNum -> SString
    textAt t i = T.singleton <$> (T.index <$> t <*> (floor . toRealFloat <$> i))
eval (AtE l i) = binaryOp atImpl l i
  where
    atImpl :: [SData] -> SNum -> SData
    atImpl xs (Just i) = (!!) xs (floor $ toRealFloat i)
    atImpl xs _ = error "index is null"
eval (UpcaseE s) = monoOp (T.toUpper <$>) s
eval (DowncaseE s) = monoOp (T.toLower <$>) s
eval (ConcatE s1 s2) = binaryOp (<>) s1 s2
eval (EqE a1 a2) = binaryOp (\a b -> Just $ a == b) a1 a2
eval (FgetE k) =
  ask >>= \ctx ->
    return $
      maybe
        (DNum Nothing)
        (\x -> featureProperties'' ctx MP.! x)
        k
eval (SDataE d) = return d
eval (ListE l) = return l
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
eval _ = error "not yet implemented"

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

-- Testing shite:
-- >>> join $ join $ fmap (\r -> runReader r <$> ctx) (eval <$> parseMaybe stringExprP "[\"get\", \"class\"]")
-- >>> fmap (\r -> runReader r <$> ctx) (eval <$> parseMaybe numExprP "[\"interpolate\",[\"exponential\", 2],2,1,2,3,6]")
