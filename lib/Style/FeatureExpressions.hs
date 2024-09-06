{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds         #-}

module Style.FeatureExpressions where

import qualified Data.Text.Lazy as T
import qualified Data.Map as MP
import Control.Lens
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Char
import Style.Parser
import Style.ExpressionsWrapper
import Style.ExpressionsContext
import Proto.Util


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

fInP :: Parser (ArgType ('SBool b))
fInP = betweenSquareBrackets $ do
  key <- betweenDoubleQuotes (string "!in" <|> string "in")
  _ <- char ',' >> space
  val <- filterByP
  _ <- char ',' >> space
  props <- SArray <$> (stringLitP `sepBy` (char ',' >> space))
  let expr = FinE val props
  if T.isPrefixOf "!" key then return $ FeatureArg $ NegationFe expr else return $ FeatureArg expr

fgetP :: Parser (ArgType (SString s))
fgetP = betweenSquareBrackets $ do
  _ <- betweenDoubleQuotes $ string "get"
  _ <- char ',' >> space
  FeatureArg . FgetE <$> stringLitP

fgeometryP :: Parser (ArgType (SString s))
fgeometryP = betweenSquareBrackets $ do
  FeatureArg FgeometryE <$ betweenDoubleQuotes (string "geometry-type")

fzoomP :: Parser (ArgType (SNum a))
fzoomP = betweenSquareBrackets $ do
  FeatureArg FzoomE <$ betweenDoubleQuotes (string "zoom")

filterParsers :: Parser (ArgType ('SBool b))
filterParsers = choice [try fInP]

evalFilterIn :: FilterBy -> SType -> ExpressionContext -> SType
evalFilterIn (FProp key) (SArray a) ctx = SBool $ maybe False (`elem` a) $  key `MP.lookup` featureProperties ctx
evalFilterIn _           _          _   = error "second argument must be an array"

evalFilterGet :: SType -> ExpressionContext -> SType
evalFilterGet (SString key) ctx = fromMaybe SNull (key `MP.lookup` featureProperties ctx)
evalFilterGet _             _   = error "get property must be of type String"

-- defaults to linestring if geometry cannot be retrieved from feature
evalGeometryType :: ExpressionContext -> SType
evalGeometryType ctx = maybe (SString "LINESTRING") SString (geometryTypeToString (ctx ^. feature))

-- TODO as soon as we have a context set up this needs to be properly impolemented
evalZoom :: ExpressionContext -> SType
evalZoom ctx = SNum (SInt (ctx ^. ctxZoom))
