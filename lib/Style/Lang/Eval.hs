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
eval (FgetE k) =
  ask >>= \ctx ->
    return $
      maybe
        (DNum Nothing)
        (\x -> featureProperties'' ctx MP.! x)
        k
eval _ = error "not yet implemented"

--------------------------------------------------------------------------------

multiOp :: ([a] -> b) -> [SExpr a] -> Reader ExpressionContext b
multiOp f a = f `fmap` traverse eval a

binaryOp :: (t -> t1 -> b) -> SExpr t -> SExpr t1 -> Reader ExpressionContext b
binaryOp f a b = eval a >>= \x -> eval b >>= \y -> return $ f x y

--------------------------------------------------------------------------------

getEval :: T.Text -> Reader ExpressionContext SData
getEval k = ask >>= \ctx -> return (featureProperties'' ctx MP.! k)

-- Testing shite:
-- >>> fmap (\r -> runReader r <$> ctx) (eval <$> parseMaybe stringExprP "[\"get\", \"class\"]")
