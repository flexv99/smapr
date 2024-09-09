{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE DataKinds      #-}

module Style.ExpressionsEval where

import qualified Data.Text.Internal.Lazy as T
import Style.ExpressionsWrapper
import Style.IsoExpressions
import Style.FeatureExpressions
import Style.ExpressionsContext
import Style.Parser

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

evalNumExpr :: ArgType (SNum n) -> ExpressionContext -> SType
evalNumExpr (IsoArg i) ctx     = evalIsoExpr i ctx
evalNumExpr (FeatureArg f) ctx = evalFeatureExpr f ctx

evalBoolExpr :: ArgType (SBool b) -> ExpressionContext -> SType
evalBoolExpr (IsoArg i) ctx     = evalIsoExpr i ctx
evalBoolExpr (FeatureArg f) ctx = evalFeatureExpr f ctx

evalStringExpr :: ArgType (SString s) -> ExpressionContext -> SType
evalStringExpr (IsoArg i) ctx     = evalIsoExpr i ctx
evalStringExpr (FeatureArg f) ctx = evalFeatureExpr f ctx

-- | evaluates an 'Expr' to the tagged type f.e.
-- >>> evalExpr $ AddE (SArray [SInt 38,SInt 4])
-- 42
evalIsoExpr :: IsoExpr res -> ExpressionContext -> SType
evalIsoExpr (Negation e)         ctx = SBool $ not $ unwrapSBool $ evalIsoExpr e ctx
evalIsoExpr (StringE s)          _   = SString s
evalIsoExpr (BoolE b)            _   = SBool b
evalIsoExpr (IntE i)             _   = SNum $ SInt i
evalIsoExpr (DoubleE d)          _   = SNum $ SDouble d
evalIsoExpr (NumE e)             _   = SNum e
evalIsoExpr (ArrayE (SArray a))  _   = SArray a
evalIsoExpr (AddE a)             ctx = stypeSum (map (`evalNumExpr` ctx) a)
evalIsoExpr (ProdE a)            ctx = stypeProd (map (`evalNumExpr` ctx) a)
evalIsoExpr (SubE a b)           ctx = stypeSub (evalNumExpr a ctx) (evalNumExpr b ctx)
evalIsoExpr (DivE a b)           ctx = stypeDiv (evalNumExpr a ctx) (evalNumExpr b ctx)
evalIsoExpr (EqE o t)            ctx = stypeEq (eval o ctx) (eval t ctx)
evalIsoExpr (AtE a i)            ctx = stypeIn a (evalNumExpr i ctx)
evalIsoExpr (AllE v)             ctx = stypeAll v ctx
evalIsoExpr (MatchE m v)         ctx = stypeMatch (eval m ctx) v
evalIsoExpr (InterpolateE t e a) ctx = stypeInterpolate t (eval (wrap e) ctx) (map (\(a', b) -> (a', eval b ctx)) a)
evalIsoExpr _                    _   = undefined

evalFeatureExpr :: FeatureExpr a -> ExpressionContext -> SType
evalFeatureExpr (NegationFe e) ctx = SBool $ not $ unwrapSBool $ evalFeatureExpr e ctx
evalFeatureExpr (FinE a b)     ctx = evalFilterIn a b ctx
evalFeatureExpr (FgetE k)      ctx = evalFilterGet k ctx
evalFeatureExpr FgeometryE     ctx = evalGeometryType ctx
evalFeatureExpr FzoomE         ctx = evalZoom ctx
evalFeatureExpr _              _   = undefined


stypeAll :: [ArgType ('SBool b)] -> ExpressionContext -> SType
stypeAll exprs ctx = SBool $ all ((\e -> unwrapSBool $ eval e ctx) . wrap) exprs

unwrapSBool :: SType -> Bool
unwrapSBool (SBool b) = b
unwrapSBool _         = error "cannot unwrap values other than bool"

unwrapSColor :: SType -> Color
unwrapSColor (SColor c) = c
unwrapSColor _          = error "cannot unwrap values other than color"

unwrapSInt :: SType -> Int
unwrapSInt (SNum (SInt i)) = i
unwrapSInt _               = error "cannot unwrap values other than int"

unwrapSDouble :: SType -> Double
unwrapSDouble (SNum (SDouble i)) = i
unwrapSDouble _                  = error "cannot unwrap values other than Double"

unwrapSString :: SType -> T.Text
unwrapSString (SString s) = s
unwrapSString _           = error "cannot unwrap values other than String"
