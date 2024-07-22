{-# LANGUAGE GADTs, KindSignatures, RankNTypes, FlexibleInstances, StandaloneDeriving, TypeOperators, DataKinds #-}

module Style.FilterExpressions where

import Data.Kind (Type)
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

data FilterBy
  = FTypeOf
  | FId Int
  | FProp T.Text
  deriving (Eq, Show)

-- | AST representation of filter expressions
data FilterExpr :: SType -> Type where
  -- | filter equality
  FeqE :: FilterBy -> SType -> FilterExpr (SBool b)

deriving instance Show (FilterExpr res)

-- | choice of possible filtering types:
-- id, type, feature properties
filterByP :: Parser FilterBy
filterByP = choice [ FId <$> pInteger
                   , FTypeOf <$ string "$type"
                   , FProp <$> pString
                   ]

feqP :: Parser (FilterExpr ('SBool b))
feqP = exprBaseP "==" $ do
  val1 <- filterByP
  _ <- char ',' >> space
  FeqE val1 <$> stringLitP


evalFilterExpr :: FilterExpr a -> Feature -> Layer -> Bool
evalFilterExpr (FeqE a b) = evalFilterEq a b

evalFilterEq :: FilterBy ->  SType -> Feature -> Layer -> Bool
evalFilterEq FTypeOf (SString s) f l     = (Just s ==) $ geometryTypeToString f
evalFilterEq (FId id) (SString s) f l    = (Just s ==) $ featureIdToString f
evalFilterEq (FProp key) (SString s) f l = undefined
