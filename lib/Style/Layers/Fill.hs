{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Style.Layers.Fill where

import Control.Lens
import qualified Data.Aeson as A
import Data.Colour
import Style.ExpressionsWrapper
import Style.Layers.Util
import Style.Parser

data FillS = FillS
  { _fillSortKey :: Maybe Int,
    _visibility :: Visibility, -- defaults to visible
    _fillAntialias :: Bool, -- defauts to true
    _fillOpacity :: IsoExpr INum, -- defaults to 1, need to support inrepolate expressions
    _fillColor :: Color, -- defaults to #000000, disabled by fill-pattern
    _fillOutlineColor :: Maybe Color, -- disabled by fill-pattern
    _fillTranslate :: Maybe SType, -- defaults to [0, 0], need to support interpolate expressions
    _fillTranslateAnchor :: TranslateAnchor
    -- , fill-pattern
  }
  deriving (Show)

makeLenses ''FillS

instance A.FromJSON FillS where
  parseJSON = A.withObject "PolygonS" $ \t ->
    FillS
      <$> t A..:? "fill-sort-key"
      <*> t A..:? "visibility" A..!= Visible
      <*> t A..:? "fill-antialias" A..!= True
      <*> (t A..:? "fill-opacity" >>= expr) A..!= NumE 1
      <*> (t A..:? "fill-color" >>= color) A..!= (black `withOpacity` 1)
      <*> (t A..:? "fill-outline-color" >>= color)
      <*> t A..:? "fill-translate"
      <*> t A..:? "fill-translate-anchor" A..!= Map

-- fill-pattern
