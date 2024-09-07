{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Style.Layers.Fill where

import qualified Data.Aeson as A
import Control.Lens
import Data.Colour
import Style.Layers.Util
import Style.Parser
import Style.ExpressionsWrapper

data FillS = FillS
  { fillSortKey           :: Maybe Int
  , visibility            :: Visibility -- defaults to visible
  , fillAntialias         :: Bool -- defauts to true
  , fillOpacity           :: WrappedExpr -- defaults to 1, need to support inrepolate expressions
  , fillColor             :: SType -- defaults to #000000, disabled by fill-pattern
  , fillOutlineColor      :: Maybe SType -- disabled by fill-pattern
  , fillTranslate         :: Maybe SType -- defaults to [0, 0], need to support interpolate expressions
  , fillTranslateAnchor   :: TranslateAnchor
  -- , fill-pattern
  } deriving (Show)
makeLenses ''FillS

instance A.FromJSON FillS where
  parseJSON = A.withObject "PolygonS" $ \t -> FillS
    <$> t A..:? "fill-sort-key"
    <*> t A..:? "visibility" A..!= Visible
    <*> t A..:? "fill-antialias" A..!= True
    <*> (t A..:? "fill-opacity" >>= expr) A..!= wrap (IsoArg $ IntE 1)
    <*> (t A..:? "fill-color" >>= color) A..!= SColor (black `withOpacity` 1)
    <*> (t A..:? "fill-outline-color" >>= color)
    <*> t A..:? "fill-translate"
    <*> t A..:? "fill-translate-anchor" A..!= Map
    -- fill-pattern
