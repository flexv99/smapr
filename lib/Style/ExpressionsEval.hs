{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE DataKinds      #-}

module Style.ExpressionsEval where

import qualified Data.Sequence as S -- for testLayerAndFeature
import Proto.Vector_tile.Tile.Feature (Feature(..))
import Proto.Vector_tile.Tile.Layer (Layer(..))
import Style.ExpressionsWrapper
import Style.IsoExpressions
import Style.FeatureExpressions
import Style.ExpressionsContext
import Style.Parser
import Proto.Util -- for testLayerAndFeature

-- | evaluates an 'WrappedExpr' to SType
-- >>> eval (IntExpr (AddE (IntE 4) (IntE 5))) f l
-- SInt 9
eval :: WrappedExpr -> ExpressionContext -> SType
eval (StringExpr (IsoArg s))     ctx = evalIsoExpr s ctx
eval (StringExpr (FeatureArg s)) ctx = evalFeatureExpr s ctx
eval (BoolExpr (IsoArg b))       ctx = evalIsoExpr b ctx
eval (BoolExpr (FeatureArg b))   ctx = evalFeatureExpr b ctx
eval (NumExpr (IsoArg i))        ctx = evalIsoExpr i ctx
eval (NumExpr (FeatureArg i))    ctx = evalFeatureExpr i ctx
eval (ArrayExpr (IsoArg a))      ctx = evalIsoExpr a ctx
eval (ArrayExpr (FeatureArg a))  ctx = evalFeatureExpr a ctx


-- | evaluates an 'Expr' to the tagged type f.e.
-- >>> evalExpr $ AddE (SArray [SInt 38,SInt 4])
-- 42
evalIsoExpr :: IsoExpr res -> ExpressionContext -> SType
evalIsoExpr (Negation e)         ctx = SBool $ not $ unwrapSBool $ evalIsoExpr e ctx
evalIsoExpr (StringE s)          ctx = SString s
evalIsoExpr (BoolE b)            ctx = SBool b
evalIsoExpr (IntE i)             ctx = SNum $ SInt i
evalIsoExpr (DoubleE d)          ctx = SNum $ SDouble d
evalIsoExpr (NumE e)             ctx = SNum e
evalIsoExpr (ArrayE (SArray a))  ctx = SArray a
evalIsoExpr (AddE a)             ctx = stypeSum (map (`eval` ctx) a)
evalIsoExpr (ProdE a)            ctx = stypeProd (map (`eval` ctx) a)
evalIsoExpr (SubE a b)           ctx = stypeSub (eval a ctx) (eval b ctx)
evalIsoExpr (DivE a b)           ctx = stypeDiv (eval a ctx) (eval b ctx)
evalIsoExpr (EqE o t)            ctx = stypeEq (eval o ctx) (eval t ctx)
evalIsoExpr (AtE a i)            ctx = stypeIn a (evalIsoExpr i ctx)
evalIsoExpr (AllE v)             ctx = stypeAll v ctx
evalIsoExpr (MatchE m v)         ctx = stypeMatch (eval m ctx) v
evalIsoExpr (InterpolateE t e a) ctx = stypeInterpolate t (eval (wrap e) ctx) (map (\(a', b) -> (a', eval (wrap b) ctx)) a)

evalFeatureExpr :: FeatureExpr a -> ExpressionContext -> SType
evalFeatureExpr (NegationFe e) ctx = SBool $ not $ unwrapSBool $ evalFeatureExpr e ctx
evalFeatureExpr (FinE a b)     ctx = evalFilterIn a b ctx
evalFeatureExpr (FgetE k)      ctx = evalFilterGet k ctx
evalFeatureExpr FgeometryE     ctx = evalGeometryType ctx
evalFeatureExpr FzoomE         ctx = evalZoom


stypeAll :: [ArgType ('SBool b)] -> ExpressionContext -> SType
stypeAll exprs ctx = SBool $ all ((\e -> unwrapSBool $ eval e ctx) . wrap) exprs

unwrapSBool :: SType -> Bool
unwrapSBool (SBool b) = b
unwrapSBool _         = error "cannot unwrap values other than bool"

