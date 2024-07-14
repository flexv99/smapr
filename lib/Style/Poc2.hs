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

pInteger :: Parser Int
pInteger = lexeme (L.signed space L.decimal) <?> "integer"

pDouble :: Parser Double
pDouble = lexeme (L.signed space L.float) <?> "float"

pBool :: Parser Bool
pBool =
  label "bool" $
    lexeme $
      (False <$ (string "false" *> notFollowedBy alphaNumChar))
        <|> (True <$ (string "true" *> notFollowedBy alphaNumChar))

pArray :: Parser [a]
pArray = undefined

-- | prefix exrp parser
--
-- >>> parseTest (prefixChainExprP (betweenDoubleQuotes $ const (+) <$> char '+') pInteger) "[\"+\",1, 2, 3]"
-- 6
prefixChainExprP :: Parser (a -> a -> a) -> Parser a -> Parser a
prefixChainExprP pOp pVal = betweenSquareBrackets $ do
  op <- pOp
  _ <- char ',' >> space
  vals <- pVal `sepBy` (char ',' >> space)
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
data Expr (res :: Type) where
  -- | string literal
  StringE :: T.Text -> Expr T.Text
  -- | bool-value
  BoolE   :: Bool -> Expr Bool
  -- | int literal
  IntE    :: Int -> Expr Int
  -- | double literal
  DoubleE :: Double -> Expr Double
  -- | strinc concatenation
  AddE    :: (Num a) => Expr a -> Expr a -> Expr a
  -- | check for equaliy on polymorphic type
  EqE     :: (Eq a) => Expr a -> Expr a -> Expr Bool
  -- | check for not equaliy on polymorphic type
  NotEqE  :: (Eq a) => Expr a -> Expr a -> Expr Bool
  -- | if-expression - condition-expression has to be of type Bool,
  --   __then__ and __else__ expression have to be the same type
  IfE     :: Expr Bool -> Expr res -> Expr res -> Expr res


-- | evaluates an 'Expr' to the tagged type
-- >>> evalExpr (AddE (IntE 10) (IfE (BoolE False) (IntE 0) (IntE 32)))
-- 42
evalExpr :: Expr res -> res
evalExpr (StringE s) = s
evalExpr (BoolE b)   = b
evalExpr (IntE i)    = i
evalExpr (DoubleE d) = d
evalExpr (AddE a b)  = evalExpr a + evalExpr b
evalExpr (EqE o t)   = evalExpr o == evalExpr t
evalExpr (IfE b t e)
  | evalExpr b = evalExpr t
  | otherwise  = evalExpr e

deriving instance Show (Expr res)

-- | runtime representation
--   mainly useful for parsing
data WrappedExpr where
  StringExpr  :: Expr T.Text -> WrappedExpr
  IntExpr     :: Expr Int -> WrappedExpr
  DoubleExpr  :: Expr Double -> WrappedExpr
  BoolExpr    :: Expr Bool -> WrappedExpr

deriving instance Show WrappedExpr

-- | evaluates an 'WrappedExpr' to SType
-- >>> eval (IntExpr (AddE (IntE 4) (IntE 5)))
-- SInt 9
eval :: WrappedExpr -> SType
eval (StringExpr s) = SString (evalExpr s)
eval (BoolExpr b)   = SBool (evalExpr b)
eval (IntExpr i)    = SInt (evalExpr i)
eval (DoubleExpr d) = SDouble (evalExpr d)

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

instance KnownResType T.Text where
  wrap = StringExpr

instance KnownResType Int where
  wrap = IntExpr

instance KnownResType Double where
  wrap = DoubleExpr

instance KnownResType Bool where
  wrap = BoolExpr

stringExprP :: Parser (Expr T.Text)
stringExprP = StringE <$> pString <* hidden space

intExprP :: Parser (Expr Int)
intExprP = IntE <$> pInteger <* hidden space

doubleExprP :: Parser (Expr Double)
doubleExprP = DoubleE <$> pDouble <* hidden space

boolExprP :: Parser (Expr Bool)
boolExprP = BoolE <$> pBool <* hidden space

-- fmap eval $ parseMaybe (boolRet2Args (betweenDoubleQuotes $ string "==") intExprP) "[\"==\",1, 1]"
boolRet2Args
  :: (Token s ~ Char, MonadParsec e s m, Eq a1) =>
     m a2 -> m (Expr a1) -> m WrappedExpr
boolRet2Args pOp pVal = betweenSquareBrackets $ do
  op <- pOp
  _ <- char ',' >> space
  val1 <- pVal
  _ <- char ',' >> space
  BoolExpr . EqE val1 <$> pVal

