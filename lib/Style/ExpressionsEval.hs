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
import Style.Parser
import Proto.Util -- for testLayerAndFeature

-- | evaluates an 'WrappedExpr' to SType
-- >>> eval (IntExpr (AddE (IntE 4) (IntE 5))) f l
-- SInt 9
eval :: WrappedExpr -> Feature -> Layer -> SType
eval (StringExpr (IsoArg s))     f l = evalIsoExpr s f l
eval (StringExpr (FeatureArg s)) f l = evalFeatureExpr s f l
eval (BoolExpr (IsoArg b))       f l = evalIsoExpr b f l
eval (BoolExpr (FeatureArg b))   f l = evalFeatureExpr b f l
eval (NumExpr (IsoArg i))        f l = evalIsoExpr i f l
eval (NumExpr (FeatureArg i))    f l = evalFeatureExpr i f l
eval (ArrayExpr (IsoArg a))      f l = evalIsoExpr a f l
eval (ArrayExpr (FeatureArg a))  f l = evalFeatureExpr a f l


-- | evaluates an 'Expr' to the tagged type f.e.
-- >>> evalExpr $ AddE (SArray [SInt 38,SInt 4])
-- 42
evalIsoExpr :: IsoExpr res -> Feature -> Layer -> SType
evalIsoExpr (Negation e)         f l = SBool $ not $ unwrapSBool $ evalIsoExpr e f l
evalIsoExpr (StringE s)          f l = SString s
evalIsoExpr (BoolE b)            f l = SBool b
evalIsoExpr (IntE i)             f l = SNum $ SInt i
evalIsoExpr (DoubleE d)          f l = SNum $ SDouble d
evalIsoExpr (NumE e)             f l = SNum e
evalIsoExpr (ArrayE (SArray a))  f l = SArray a
evalIsoExpr (AddE a)             f l = stypeSum (map (\x -> eval x f l) a)
evalIsoExpr (ProdE a)            f l = stypeProd (map (\x -> eval x f l) a)
evalIsoExpr (SubE a b)           f l = stypeSub (eval a f l) (eval b f l)
evalIsoExpr (DivE a b)           f l = stypeDiv (eval a f l) (eval b f l)
evalIsoExpr (EqE o t)            f l = stypeEq (eval o f l) (eval t f l)
evalIsoExpr (AtE a i)            f l = stypeIn a (evalIsoExpr i f l)
evalIsoExpr (AllE v)             f l = stypeAll v f l
evalIsoExpr (MatchE m v)         f l = stypeMatch (eval m f l) v
evalIsoExpr (InterpolateE t e a) f l = stypeInterpolate t (eval (wrap e) f l) (map (\(a', b) -> (a', eval (wrap b) f l)) a)

evalFeatureExpr :: FeatureExpr a -> Feature -> Layer -> SType
evalFeatureExpr (NegationFe e) f l = SBool $ not $ unwrapSBool $ evalFeatureExpr e f l
evalFeatureExpr (FinE a b)     f l = evalFilterIn a b f l
evalFeatureExpr (FgetE k)      f l = evalFilterGet k f l
evalFeatureExpr FgeometryE     f l = evalGeometryType f
evalFeatureExpr FzoomE         f l = evalZoom


stypeAll :: [ArgType ('SBool b)] -> Feature -> Layer -> SType
stypeAll exprs f l = SBool $ all ((\e -> unwrapSBool $ eval e f l) . wrap) exprs

unwrapSBool :: SType -> Bool
unwrapSBool (SBool b) = b
unwrapSBool _         = error "cannot unwrap values other than bool"

