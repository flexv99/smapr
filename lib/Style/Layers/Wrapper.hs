{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Style.Layers.Wrapper where

import Control.Monad.Reader
import qualified Data.Aeson as A
import qualified Data.Aeson.Text as A
import qualified Data.Aeson.Types as A
import Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy as T
import Lens.Micro
import Lens.Micro.TH
import Style.ExpressionsContext
import Style.Lang.Ast
import Style.Lang.Eval
import Style.Lang.Parser
import Style.Lang.Types
import Style.Layers.Background
import Style.Layers.Fill
import Style.Layers.Line
import Style.Layers.Point
import Text.Megaparsec

data Paint
  = LinePaint LineS
  | FillPaint FillS
  | BackgroundPaint BackgroundS
  | PointPaint PointS
  deriving (Show)

data SLayer = SLayer
  { _id :: T.Text
  , _pType :: T.Text
  , _source :: Maybe T.Text
  , _sourceLayer :: Maybe T.Text
  , _lfilter :: Maybe (SExpr SBool)
  , _paint :: Maybe Paint
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
    SLayer id' type' source' sourceLayer' filter'
      <$> (sequenceA $ paintP type' <$> p)
    where
      fexpr :: Maybe A.Value -> A.Parser (Maybe (SExpr SBool))
      fexpr Nothing = pure Nothing
      fexpr (Just v) = case parse boolExprP "" (A.encodeToLazyText v) of
        Left err -> fail $ errorBundlePretty err
        Right res -> pure $ Just res
      paintP t = A.withObject "Paint" $ \v -> do
        case t of
          "line" -> LinePaint <$> A.parseJSON (A.Object v)
          "fill" -> FillPaint <$> A.parseJSON (A.Object v)
          "background" -> BackgroundPaint <$> A.parseJSON (A.Object v)
          "symbol" -> PointPaint <$> A.parseJSON (A.Object v)
          _ -> FillPaint <$> A.parseJSON (A.Object v) -- not sure about this case

evalLayer :: SLayer -> Reader ExpressionContext SBool
evalLayer (SLayer{_lfilter = Just fltr}) = eval fltr
evalLayer _ = return $ Just True
