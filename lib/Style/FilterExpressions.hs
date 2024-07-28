{-# LANGUAGE GADTs, KindSignatures, RankNTypes, FlexibleInstances, StandaloneDeriving, TypeOperators, DataKinds #-}

module Style.FilterExpressions where

import Data.Kind (Type)
import Data.Functor ((<&>))
import Proto.Vector_tile.Tile.Feature (Feature(..))
import Proto.Vector_tile.Tile.Layer (Layer(..))
import qualified Data.Text.Lazy as T
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Sequence as S
import qualified Data.Map as MP
import Text.Megaparsec
import Text.Megaparsec.Char
import Style.Parser
import Style.Expressions
import Proto.Util
import ApiClient

data FilterBy
  = FTypeOf
  | FId Int
  | FProp T.Text
  deriving (Eq, Show)

-- | AST representation of filter expressions
data FilterExpr :: SType -> Type where
  StringFe :: T.Text -> FilterExpr (SString s)
  ArrayFe  :: SType -> FilterExpr (SArray a)
  Negation :: FilterExpr (SBool s) -> FilterExpr (SBool b)
  -- | filter by equality
  FeqE  :: FilterBy -> SType -> FilterExpr (SBool b)
  -- | in lookup
  FinE  :: FilterBy -> SType -> FilterExpr (SBool b)

deriving instance Show (FilterExpr res)

typeParser :: Parser T.Text
typeParser = label "type" $ betweenDoubleQuotes $ do
  _ <- char '$'
  string "type"

-- | choice of possible filtering types:
-- id, type, feature properties
filterByP :: Parser FilterBy
filterByP = choice [ FId <$> pInteger
                   , try $ FTypeOf <$ typeParser
                   , FProp <$> pString
                   ]

fEqP :: Parser (FilterExpr ('SBool b))
fEqP = betweenSquareBrackets $ do
  key <- betweenDoubleQuotes (string "!=" <|> string "==")
  _ <- char ',' >> space
  val1 <- filterByP
  _ <- char ',' >> space
  val2 <- stringLitP
  let expr = FeqE val1 val2
  if T.isPrefixOf "!" key then return $ Negation expr else return expr

fInP :: Parser (FilterExpr ('SBool b))
fInP = betweenSquareBrackets $ do
  key <- betweenDoubleQuotes (string "!in" <|> string "in")
  _ <- char ',' >> space
  val <- filterByP
  _ <- char ',' >> space
  props <- SArray <$> (stringLitP `sepBy` (char ',' >> space))
  let expr = FinE val props
  if T.isPrefixOf "!" key then return $ Negation expr else return expr


evalFilterExpr :: FilterExpr a -> Feature -> Layer -> Bool
evalFilterExpr (Negation e) f l = not $ evalFilterExpr e f l
evalFilterExpr (FeqE a b)   f l = evalFilterEq a b f l
evalFilterExpr (FinE a b)   f l = evalFilterIn a b f l

evalFilterEq :: FilterBy -> SType -> Feature -> Layer -> Bool
evalFilterEq FTypeOf (SString s) f l     = (Just (T.toCaseFold s) ==) $ T.toCaseFold <$> geometryTypeToString f
evalFilterEq (FId id) (SString s) f l    = (Just s ==) $ featureIdToString f
evalFilterEq (FProp key) (SString s) f l = (Just s ==) $ key `MP.lookup` featureProperties l f
evalFilterEq _ _ f l                     = error "wrong params"

evalFilterIn :: FilterBy -> SType -> Feature -> Layer -> Bool
evalFilterIn (FProp key) (SArray a) f l = maybe False (\v -> any (T.isInfixOf v . T.pack . show) a) $  key `MP.lookup` featureProperties l f


