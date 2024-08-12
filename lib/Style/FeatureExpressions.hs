{-# LANGUAGE GADTs, KindSignatures, RankNTypes, FlexibleInstances, StandaloneDeriving, DataKinds #-}

module Style.FeatureExpressions where

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
import Style.ExpressionsWrapper
import Proto.Util
import ApiClient


typeParser :: Parser T.Text
typeParser = label "type" $ betweenSquareBrackets $ betweenDoubleQuotes $ do
  string "geometry-type"

-- | choice of possible filtering types:
-- id, type, feature properties
filterByP :: Parser FilterBy
filterByP = choice [ FId <$> pInteger
                   , try $ FTypeOf <$ typeParser
                   , FProp <$> pString
                   ]

fEqP :: Parser (FeatureExpr ('SBool b))
fEqP = betweenSquareBrackets $ do
  key <- betweenDoubleQuotes (string "!=" <|> string "==")
  _ <- char ',' >> space
  val1 <- filterByP
  _ <- char ',' >> space
  val2 <- stringLitP
  let expr = FeqE val1 val2
  if T.isPrefixOf "!" key then return $ Negation expr else return expr

fInP :: Parser (FeatureExpr ('SBool b))
fInP = betweenSquareBrackets $ do
  key <- betweenDoubleQuotes (string "!in" <|> string "in")
  _ <- char ',' >> space
  val <- filterByP
  _ <- char ',' >> space
  props <- SArray <$> (stringLitP `sepBy` (char ',' >> space))
  let expr = FinE val props
  if T.isPrefixOf "!" key then return $ Negation expr else return expr

fAllP :: Parser (FeatureExpr ('SBool a))
fAllP = betweenSquareBrackets $ do
  _ <- betweenDoubleQuotes $ string "all"
  _ <- char ',' >> space
  FallE <$> filterParsers `sepBy` (char ',' >> space)

fgetP :: Parser (FeatureExpr (SString s))
fgetP = betweenSquareBrackets $ do
  _ <- betweenDoubleQuotes $ string "get"
  _ <- char ',' >> space
  FgetE <$> stringLitP

fgeometryP :: Parser (FeatureExpr (SString s))
fgeometryP = betweenSquareBrackets $ do
  FgeometryE <$ betweenDoubleQuotes (string "geometry-type")

filterParsers :: Parser (FeatureExpr ('SBool b))
filterParsers = choice [try fAllP, try fEqP, try fInP]

evalFeatureExpr :: FeatureExpr a -> Feature -> Layer -> SType
evalFeatureExpr (Negation e) f l = SBool $ not $ unwrapSBool $ evalFeatureExpr e f l
evalFeatureExpr (FeqE a b)   f l = evalFilterEq a b f l
evalFeatureExpr (FinE a b)   f l = evalFilterIn a b f l
evalFeatureExpr (FallE v)    f l = evalAll v f l
evalFeatureExpr (FgetE k)    f l = evalFilterGet k f l
evalFeatureExpr FgeometryE   f l = evalGeometryType f

evalFilterEq :: FilterBy -> SType -> Feature -> Layer -> SType
evalFilterEq FTypeOf (SString s)     f l = SBool $ (Just (T.toCaseFold s) ==) $ T.toCaseFold <$> geometryTypeToString f
evalFilterEq (FId id) (SString s)    f l = SBool $ (Just s ==) $ featureIdToString f
evalFilterEq (FProp key) (SString s) f l = SBool $ (Just s ==) $ key `MP.lookup` featureProperties l f
evalFilterEq _ _ f l                     = error "wrong params"

evalFilterIn :: FilterBy -> SType -> Feature -> Layer -> SType
evalFilterIn (FProp key) (SArray a) f l = SBool $ maybe False (\v -> any (T.isInfixOf v . T.pack . show) a) $  key `MP.lookup` featureProperties l f

evalFilterGet :: SType -> Feature -> Layer -> SType
evalFilterGet (SString key) f l = maybe SNull SString (key `MP.lookup` featureProperties l f)
evalFilterGet _             f l = error "get property must be of type String"

evalAll :: [FeatureExpr ('SBool b)] -> Feature -> Layer -> SType
evalAll exprs f l = SBool $ all (\e -> unwrapSBool $ evalFeatureExpr e f l) exprs

-- defaults to linestring if geometry cannot be retrieved from feature
evalGeometryType :: Feature -> SType
evalGeometryType f = maybe (SString "LINESTRING") SString (geometryTypeToString f)

unwrapSBool :: SType -> Bool
unwrapSBool (SBool b) = b
unwrapSBool _         = error "cannot unwrap values other than bool"
