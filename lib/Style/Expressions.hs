{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures#-}
{-# LANGUAGE RankNTypes#-}
{-# LANGUAGE FlexibleInstances#-}
{-# LANGUAGE StandaloneDeriving#-}
{-# LANGUAGE DataKinds #-}

module Style.Expressions where

import Data.Kind (Type)
import qualified Data.Text.Lazy as T
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec
import Text.Megaparsec.Char
import Style.Parser

-- | AST representation of map libre's style spec expressions
--   value-type the expression is representing (see 'evalExpr')
data Expr :: SType -> Type where
  -- | string literal
  StringE :: T.Text -> Expr (SString s)
  -- | bool-value
  BoolE   :: Bool -> Expr (SBool b)
  -- | int literal
  IntE    :: Int -> Expr (SInt i)
  -- | double literal
  DoubleE :: Double -> Expr (SDouble d)
  -- | list literal
  ArrayE  :: SType -> Expr (SArray a)
  -- | addition
  AddE    :: SType -> Expr a
  -- | product
  ProdE   :: SType -> Expr a
  -- | subtraction
  SubE    :: SType -> SType -> Expr a
  -- | division
  DivE    :: SType -> SType -> Expr a
  -- | check for equaliy on polymorphic types
  EqE     :: WrappedExpr -> WrappedExpr -> Expr (SBool b)
  -- | element at index
  AtE    :: SType -> Expr (SInt i) -> Expr a

deriving instance Show (Expr res)

-- | runtime representation
--   mainly useful for parsing
data WrappedExpr where
  StringExpr  :: Expr (SString n)  -> WrappedExpr
  IntExpr     :: Expr (SInt i)     -> WrappedExpr
  DoubleExpr  :: Expr (SDouble d)  -> WrappedExpr
  BoolExpr    :: Expr (SBool b)    -> WrappedExpr
  ArrayExpr   :: Expr (SArray a)   -> WrappedExpr

deriving instance Show WrappedExpr

-- | helps wrapping 'Expr' to the right
--   'WrappedExpr' constructor
--
-- >>> wrap (IntE 42)
-- IntExpr (IntE 42)
--
-- >>> wrap (IsNullE (IntE 42))
-- BoolExpr (IsNullE (IntE 42))
class KnownResType a where
  wrap :: Expr a -> WrappedExpr

instance KnownResType (SString b) where
  wrap = StringExpr

instance KnownResType (SInt a) where
  wrap = IntExpr

instance KnownResType (SDouble d) where
  wrap = DoubleExpr

instance KnownResType (SBool b) where
  wrap = BoolExpr

instance KnownResType (SArray a) where
  wrap = ArrayExpr

stringExprP :: Parser (Expr (SString s))
stringExprP = StringE <$> pString <* hidden space

intExprP :: Parser (Expr (SInt i))
intExprP = IntE <$> pInteger <* hidden space

doubleExprP :: Parser (Expr (SDouble d))
doubleExprP = DoubleE <$> pDouble <* hidden space

boolExprP :: Parser (Expr (SBool b))
boolExprP = BoolE <$> pBool <* hidden space

arrayExprP :: Parser (Expr (SArray a))
arrayExprP = ArrayE <$> arrayLitP <* hidden space

exprChoicheP :: Parser WrappedExpr
exprChoicheP = choice [ wrap <$> intExprP
                      , wrap <$> doubleExprP
                      , wrap <$> boolExprP
                      , wrap <$> stringExprP
                      , wrap <$> arrayExprP]

-- | evaluates an 'Expr' to the tagged type f.e.
-- >>> evalExpr $ AddE (SArray [SInt 38,SInt 4])
-- 42
evalExpr :: Expr res -> SType
evalExpr (StringE s)          = SString s
evalExpr (BoolE b)            = SBool b
evalExpr (IntE i)             = SInt i
evalExpr (DoubleE d)          = SDouble d
evalExpr (ArrayE (SArray a))  = SArray a
evalExpr (AddE (SArray a))    = stypeSum a
evalExpr (ProdE (SArray a))   = stypeProd a
evalExpr (SubE a b)           = stypeSub a b
evalExpr (DivE a b)           = stypeDiv a b
evalExpr (EqE o t)            = stypeEq (eval o) (eval t)
evalExpr (AtE a i)            = stypeIn a (evalExpr i)
-- evalExpr (IfE b t e)
--   | evalExpr b = evalExpr t
--   | otherwise  = evalExpr e

-- | evaluates an 'WrappedExpr' to SType
-- >>> eval (IntExpr (AddE (IntE 4) (IntE 5)))
-- SInt 9
eval :: WrappedExpr -> SType
eval (StringExpr s) = evalExpr s
eval (BoolExpr b)   = evalExpr b
eval (IntExpr i)    = evalExpr i
eval (DoubleExpr d) = evalExpr d
eval (ArrayExpr a)  = evalExpr a

stypeSum :: [SType] -> SType
stypeSum = foldr stypeAdd (SInt 0)
  where
    stypeAdd :: SType -> SType -> SType
    stypeAdd (SInt i)    (SInt j)     = SInt $ i + j
    stypeAdd (SInt i)    (SDouble j)  = SDouble $ fromIntegral i + j
    stypeAdd (SDouble i) (SInt j)     = SDouble $ i + fromIntegral j
    stypeAdd (SDouble i) (SDouble j)  = SDouble $ i + j
    stypeAdd _ _                      = error "must be numeric type"

stypeProd :: [SType] -> SType
stypeProd = foldr stypeProd (SInt 1)
  where
    stypeProd :: SType -> SType -> SType
    stypeProd (SInt i)    (SInt j)     = SInt $ i * j
    stypeProd (SInt i)    (SDouble j)  = SDouble $ fromIntegral i * j
    stypeProd (SDouble i) (SInt j)     = SDouble $ i * fromIntegral j
    stypeProd (SDouble i) (SDouble j)  = SDouble $ i * j
    stypeProd _ _                      = error "must be numeric type"

stypeSub :: SType -> SType -> SType
stypeSub (SInt i)    (SInt j)    = SInt $ i - j
stypeSub (SInt i)    (SDouble j) = SDouble $ fromIntegral i - j
stypeSub (SDouble i) (SInt j)    = SDouble $ i - fromIntegral j
stypeSub (SDouble i) (SDouble j) = SDouble $ i - j
stypeSub _ _                     = error "must be numeric type"

stypeDiv :: SType -> SType -> SType
stypeDiv (SInt i)    (SInt j)    = SDouble $ fromIntegral i / fromIntegral j
stypeDiv (SInt i)    (SDouble j) = SDouble $ fromIntegral i / j
stypeDiv (SDouble i) (SInt j)    = SDouble $ i / fromIntegral j
stypeDiv (SDouble i) (SDouble j) = SDouble $ i / j
stypeDiv _ _                     = error "must be numeric type"

stypeEq :: SType -> SType -> SType
stypeEq (SInt i)    (SInt j)    = SBool $ i == j
stypeEq (SDouble i) (SDouble j) = SBool $ i == j
stypeEq (SString i) (SString j) = SBool $ i == j
stypeEq (SBool i)   (SBool j)   = SBool $ i == j
stypeEq (SArray i)  (SArray j)  = SBool $ i == j
stypeEq _ _                     = error "eq on not supported types"

stypeIn :: SType -> SType -> SType
stypeIn (SArray a) (SInt i) = a !! i
stypeIn _          (SInt i) = error "param 1 must be an array"
stypeIn (SArray a) _        = error "param 2 must be an int"

-- >>> fmap evalExpr $ parseMaybe numRetExprP "[\"+\", 1, [\"/\", 1, 2]]"
-- 1.5
numRetExprP :: Parser (Expr a)
numRetExprP = choice $ map try [ AddE . SArray <$> exprBaseP "+"  (singleArgP  `sepBy` (char ',' >> space))
                               , exprBaseP "-" $ SubE <$> argWithComma <*> singleArgP
                               , ProdE . SArray <$> exprBaseP "*" (singleArgP `sepBy` (char ',' >> space))
                               , exprBaseP "/" $ DivE <$> argWithComma <*> singleArgP
                               ]
              where
                singleArgP = numberLitP <|> (evalExpr <$> numRetExprP)
                argWithComma = do
                  val <- singleArgP
                  _ <- char ',' >> space
                  return val

-- >>> fmap evalExpr $ parseMaybe eqP "[\"==\", [1, 2, 3], [123]]"
-- false
-- >> evalExpr <$> parseMaybe eqP "[\"==\", [\"+\", 123, 4], 127]"
-- true
eqP :: Parser (Expr ('SBool b))
eqP = exprBaseP "==" $ do
  val1 <- try exprChoicheP <|> (wrap <$> (numRetExprP :: (Parser (Expr (SInt i)))))
  _ <- char ',' >> space
  EqE val1 <$> exprChoicheP


atP :: Parser (Expr a)
atP = exprBaseP "at" $ do
  val1 <- arrayLitP
  _ <- char ',' >> space
  AtE val1 <$> intExprP
