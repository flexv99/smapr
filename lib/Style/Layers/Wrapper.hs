{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Style.Layers.Wrapper where

import Control.Lens
import Control.Monad.Reader
import qualified Data.Aeson as A
import qualified Data.Aeson.Text as A
import qualified Data.Aeson.Types as A
import qualified Data.Text.Lazy as T
import Style.ExpressionsContext
import Style.ExpressionsWrapper
import Style.IsoExpressions
import Style.Layers.Background
import Style.Layers.Fill
import Style.Layers.Line
import Text.Megaparsec

data Paint = LinePaint LineS | FillPaint FillS | BackgroundPaint BackgroundS deriving (Show)

data SLayer = SLayer
  { _id :: T.Text,
    _pType :: T.Text,
    _source :: Maybe T.Text,
    _sourceLayer :: Maybe T.Text,
    _lfilter :: Maybe (IsoExpr Bool),
    _paint :: Maybe Paint
  }

makeLenses ''SLayer

deriving instance Show SLayer

instance A.FromJSON SLayer where
  parseJSON = A.withObject "SLayer" $ \obj -> do
    id' <- obj A..: "id"
    type' <- obj A..: "type"
    source' <- obj A..:? "source"
    sourceLayer' <- obj A..:? "source-layer"
    filter' <- obj A..:? "filter" >>= fexpr
    p <- obj A..:? "paint"
    let p' = sequenceA $ paintP type' <$> p
    SLayer id' type' source' sourceLayer' filter' <$> p'
    where
      fexpr :: Maybe A.Value -> A.Parser (Maybe (IsoExpr Bool))
      fexpr Nothing = pure Nothing
      fexpr (Just v) = case parse boolExprP "" (A.encodeToLazyText v) of
        Left err -> fail $ errorBundlePretty err
        Right res -> pure $ Just res
      paintP t = A.withObject "Paint" $ \v -> do
        case t of
          "line" -> LinePaint <$> A.parseJSON (A.Object v)
          "fill" -> FillPaint <$> A.parseJSON (A.Object v)
          "background" -> BackgroundPaint <$> A.parseJSON (A.Object v)
          _ -> FillPaint <$> A.parseJSON (A.Object v) -- not sure about this case

evalLayer :: SLayer -> Reader ExpressionContext Bool
evalLayer (SLayer {_lfilter = Just fltr}) = eval fltr
evalLayer _ = return True
