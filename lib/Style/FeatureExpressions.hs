{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}

module Style.FeatureExpressions where

import Data.Kind (Type)
import Data.Functor ((<&>))
import Proto.Vector_tile.Tile.Feature (Feature(..))
import Proto.Vector_tile.Tile.Layer (Layer(..))
import qualified Data.Text.Lazy as T
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Sequence as S
import qualified Data.Map as MP
import Data.Maybe
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

fInP :: Parser (FeatureExpr ('SBool b))
fInP = betweenSquareBrackets $ do
  key <- betweenDoubleQuotes (string "!in" <|> string "in")
  _ <- char ',' >> space
  val <- filterByP
  _ <- char ',' >> space
  props <- SArray <$> (stringLitP `sepBy` (char ',' >> space))
  let expr = FinE val props
  if T.isPrefixOf "!" key then return $ NegationFe expr else return expr

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
filterParsers = choice [try fAllP, try fInP]

evalFilterIn :: FilterBy -> SType -> Feature -> Layer -> SType
evalFilterIn (FProp key) (SArray a) f l = SBool $ maybe False (`elem` a) $  key `MP.lookup` featureProperties l f

evalFilterGet :: SType -> Feature -> Layer -> SType
evalFilterGet (SString key) f l = fromMaybe SNull (key `MP.lookup` featureProperties l f)
evalFilterGet _             f l = error "get property must be of type String"

evalAll :: [FeatureExpr ('SBool b)] -> Feature -> Layer -> SType
evalAll exprs f l = undefined -- SBool $ all (\e -> unwrapSBool $ eval e f l) exprs

-- defaults to linestring if geometry cannot be retrieved from feature
evalGeometryType :: Feature -> SType
evalGeometryType f = maybe (SString "LINESTRING") SString (geometryTypeToString f)
