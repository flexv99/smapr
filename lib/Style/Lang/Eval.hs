{-# LANGUAGE MonoLocalBinds #-}

module Style.Lang.Eval (eval) where

import Control.Monad.Reader
import Data.Functor
import Data.Functor.Identity
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
eval (AddE a) = multiOp (fmap sum . sequence) a
eval (ProdE p) = multiOp (fmap product . sequence) p
eval (SubE s1 s2) = binaryOp (\a b -> (-) <$> a <*> b) s1 s2
eval (DivE s1 s2) = binaryOp (\a b -> (/) <$> a <*> b) s1 s2
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

-- Testing shite:
-- >>> join $ join $ fmap (\r -> runReader r <$> ctx) (eval <$> parseMaybe stringExprP "[\"get\", \"class\"]")
