{-# LANGUAGE GADTs, KindSignatures, RankNTypes, FlexibleInstances, StandaloneDeriving, TypeOperators, DataKinds #-}

module Style.Poc2 where

import Data.Kind (Type)
import Data.Void
import qualified Data.Text.Lazy as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- POC for parsing expressions

type Parser = Parsec Void T.Text

data SType
  = SInt Int
  | SDouble Double
  | SString T.Text
  | SBool Bool
  | SArray [SType]
  | STypeType T.Text
  deriving (Show, Eq)

-- General Parser
-- the space consumer
sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: T.Text -> Parser T.Text
symbol = L.symbol sc

-- parse snake case property names
snakeCaseChar :: Parser Char
snakeCaseChar = alphaNumChar <|> char '_'

betweenBrackets :: (Token s ~ Char, MonadParsec e s m) => m a -> m a
betweenBrackets = between (char '(' >> space) (char ')' >> space)

betweenSquareBrackets :: (Token s ~ Char, MonadParsec e s m) => m a -> m a
betweenSquareBrackets = between (char '[' >> space) (char ']' >> space)

betweenDoubleQuotes :: (Token s ~ Char, MonadParsec e s m) => m a -> m a
betweenDoubleQuotes = between (char '"' >> space) (char '"' >> space)

pKeyword :: T.Text -> Parser T.Text
pKeyword keyword =
  label ("property_key: " ++ T.unpack keyword) $
    betweenDoubleQuotes $
      lexeme (string keyword <* notFollowedBy alphaNumChar)

pString :: Parser T.Text
pString = fmap
      T.pack
      (betweenDoubleQuotes
          (lexeme (many snakeCaseChar) <?> "string"))

pBool :: Parser Bool
pBool =
  label "bool" $
    lexeme $
      (False <$ (string "false" *> notFollowedBy alphaNumChar))
        <|> (True <$ (string "true" *> notFollowedBy alphaNumChar))

pInteger :: Parser Int
pInteger = lexeme (L.signed space L.decimal) <?> "integer"

pDouble :: Parser Double
pDouble = lexeme (L.signed space L.float) <?> "float"

pArray :: Parser a -> Parser [a]
pArray pAtom = betweenSquareBrackets
    (pAtom `sepBy` (char ',' >> space))


stringLitP :: Parser SType
stringLitP = SString <$> pString

boolLitP :: Parser SType
boolLitP = SBool <$> pBool

intLitP :: Parser SType
intLitP = SInt <$> pInteger

doubleLitP :: Parser SType
doubleLitP = SDouble <$> pDouble

pNumber :: Parser SType
pNumber = try doubleLitP <|> intLitP <?> "number"

pAtom :: Parser SType
pAtom =
  try $
    choice
      [ pNumber
      , boolLitP
      , stringLitP
      ]

arrayLitP :: Parser SType
arrayLitP = SArray <$> betweenSquareBrackets
    (pAtom `sepBy` (char ',' >> space))


-- | prefix exrp parser
--
-- >>> parseTest (prefixChainExprP (betweenDoubleQuotes $ const (+) <$> char '+') pInteger) "[\"+\",1, 2, 3]"
-- 6
prefixChainExprP :: Parser (a -> a -> a) -> Parser a -> Parser a
prefixChainExprP pOp pVal = betweenSquareBrackets $ do
  op <- pOp
  _ <- char ',' >> space
  vals <- (pVal <|> prefixChainExprP pOp pVal) `sepBy` (char ',' >> space)
  return (foldl1 op vals)

prefixBoolExprP :: (Eq a) => Parser (a -> a -> Bool) -> Parser a -> Parser Bool
prefixBoolExprP pOp pVal = betweenSquareBrackets $ do
  op <- pOp
  _ <- char ',' >> space
  val1 <- pVal
  _ <- char ',' >> space
  val2 <- pVal
  return (val1 `op` val2)

-- | AST representation of our little domain language tagged by the
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
  AddE    :: Expr (SInt i) -> Expr (SInt i) -> Expr (SInt i)
  -- | check for equaliy on polymorphic types
  EqE     :: WrappedExpr -> WrappedExpr -> Expr (SBool b)
  -- | get
  AtE    :: SType -> Expr (SInt i) -> Expr a

-- | evaluates an 'Expr' to the tagged type
-- >>> evalExpr (AddE (IntE 10) (IfE (BoolE False) (IntE 0) (IntE 32)))
-- 42
evalExpr :: Expr res -> SType
evalExpr (StringE s)          = SString s
evalExpr (BoolE b)            = SBool b
evalExpr (IntE i)             = SInt i
evalExpr (DoubleE d)          = SDouble d
evalExpr (ArrayE (SArray a))  = SArray a
evalExpr (AddE a b)           = stypeSum (evalExpr a) (evalExpr b)
evalExpr (EqE o t)            = stypeEq (eval o) (eval t)
evalExpr (AtE a i)            = sIn a (evalExpr i)
-- evalExpr (IfE b t e)
--   | evalExpr b = evalExpr t
--   | otherwise  = evalExpr e

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

-- | evaluates an 'WrappedExpr' to SType
-- >>> eval (IntExpr (AddE (IntE 4) (IntE 5)))
-- SInt 9
eval :: WrappedExpr -> SType
eval (StringExpr s) = evalExpr s
eval (BoolExpr b)   = evalExpr b
eval (IntExpr i)    = evalExpr i
eval (DoubleExpr d) = evalExpr d
eval (ArrayExpr a)  = evalExpr a

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

unified :: Parser WrappedExpr
unified = fmap wrap intExprP <|> fmap wrap boolExprP <|> fmap wrap doubleExprP <|> fmap wrap stringExprP <|> fmap wrap arrayExprP

stypeSum :: SType -> SType -> SType
stypeSum (SInt i) (SInt j) = SInt $ i + j
stypeSum _ _               = error "sum accepts numbers only"

stypeEq :: SType -> SType -> SType
stypeEq (SInt i) (SInt j)       = SBool $ i == j
stypeEq (SDouble i) (SDouble j) = SBool $ i == j
stypeEq (SString i) (SString j) = SBool $ i == j
stypeEq (SBool i) (SBool j)     = SBool $ i == j
stypeEq (SArray i) (SArray j)   = SBool $ i == j
stypeEq _ _                     = error "eq on nor supported types"

sIn :: SType -> SType -> SType
sIn (SArray a) (SInt i) = a !! i
sIn _ (SInt i) = error "param 1 must be an array"
sIn (SArray a) _ = error "param 2 must be an int"


-- TODO accept n args
-- >>> fmap evalExpr $ parseMaybe testSumP "[\"+\", 1, [\"+\", 1, 2]]"
-- 4
testSumP :: Parser (Expr ('SInt i))
testSumP = betweenSquareBrackets $ do
  _ <- betweenDoubleQuotes $ string "+"
  _ <- char ',' >> space
  val1 <- intExprP <|> testSumP
  _ <- char ',' >> space
  val2 <- intExprP <|> testSumP
  return $ AddE val1 val2

-- >>> fmap evalExpr $ parseMaybe testEqP "[\"==\", 1, 1]"
-- true
-- >>> fmap evalExpr $ parseMaybe testEqP "[\"==\", [1, 2, 3], [123]]"
-- false
-- >> evalExpr <$> parseMaybe testEqP "[\"==\", [\"+\", 123, 4], 127]"
-- true
testEqP :: Parser (Expr ('SBool b))
testEqP = betweenSquareBrackets $ do
  _ <- betweenDoubleQuotes $ string "=="
  _ <- char ',' >> space
  val1 <- wrap <$> testSumP <|> unified
  _ <- char ',' >> space
  EqE val1 <$> (wrap <$> testSumP <|> unified)


testAtP :: Parser (Expr a)
testAtP = betweenSquareBrackets $ do
  _ <- betweenDoubleQuotes $ string "at"
  _ <- char ',' >> space
  val1 <- arrayLitP
  _ <- char ',' >> space
  AtE val1 <$> intExprP
