{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Style.FeatureExpressions where

import Control.Lens
import qualified Data.Map as MP
import Data.Maybe
import qualified Data.Text.Lazy as T
import Proto.Util
import Style.ExpressionsContext
import Style.ExpressionsWrapper
import Style.Parser
import Text.Megaparsec
import Text.Megaparsec.Char

typeParser :: Parser T.Text
typeParser = label "type" $ betweenSquareBrackets $ betweenDoubleQuotes $ do
  string "geometry-type"

-- | choice of possible filtering types:
-- id, type, feature properties
filterByP :: Parser FilterBy
filterByP =
  choice
    [ FId <$> pInteger,
      try $ FTypeOf <$ typeParser,
      FProp <$> pString
    ]

fgetP :: Parser (ArgType a)
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

evalFilterIn :: FilterBy -> SType -> ExpressionContext -> SType
evalFilterIn (FProp key) (SArray a) ctx = SBool $ maybe False (`elem` a) $ key `MP.lookup` featureProperties ctx
evalFilterIn _ _ _ = error "second argument must be an array"

evalFilterGet :: SType -> ExpressionContext -> SType
evalFilterGet (SString key) ctx = fromMaybe SNull (key `MP.lookup` featureProperties ctx)
evalFilterGet _ _ = error "get property must be of type String"

-- defaults to linestring if geometry cannot be retrieved from feature
evalGeometryType :: ExpressionContext -> SType
evalGeometryType ctx = maybe (SString "LINESTRING") SString (geometryTypeToString (ctx ^. feature))

evalZoom :: ExpressionContext -> SType
evalZoom ctx = SNum (SDouble (ctx ^. ctxZoom))
