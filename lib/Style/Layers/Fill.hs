{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Style.Layers.Fill where

import qualified Data.Aeson as A
import Data.Colour
import Lens.Micro
import Lens.Micro.TH
import Style.Lang.Ast
import Style.Lang.Parser
import Style.Lang.Types
import Style.Layers.Util

data FillS = FillS
  { _fillSortKey :: Maybe Int
  , _visibility :: Visibility -- defaults to visible
  , _fillAntialias :: SExpr SBool -- defauts to true
  , _fillOpacity :: SExpr SNum -- defaults to 1, need to support inrepolate expressions
  , _fillColor :: SExpr SColor -- defaults to #000000, disabled by fill-pattern
  , _fillOutlineColor :: Maybe (SExpr SColor) -- disabled by fill-pattern
  , _fillTranslate :: Maybe SData -- defaults to [0, 0], need to support interpolate expressions
  , _fillTranslateAnchor :: TranslateAnchor
  -- , fill-pattern
  }
  deriving (Show)

makeLenses ''FillS

instance A.FromJSON FillS where
  parseJSON = A.withObject "PolygonS" $ \t ->
    FillS
      <$> t A..:? "fill-sort-key"
      <*> t A..:? "visibility" A..!= Visible
      <*> (t A..:? "fill-antialias" >>= bexpr) A..!= BoolE (Just True)
      <*> (t A..:? "fill-opacity" >>= expr) A..!= NumE (Just 1)
      <*> (t A..:? "fill-color" >>= color) A..!= ColorE (Just (black `withOpacity` 1))
      <*> (t A..:? "fill-outline-color" >>= color)
      <*> t A..:? "fill-translate"
      <*> t A..:? "fill-translate-anchor" A..!= Map

-- fill-pattern
