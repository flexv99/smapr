{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Style.Layers.Background where

import Control.Lens
import qualified Data.Aeson as A
import Data.Colour
import Style.ExpressionsWrapper
import Style.Layers.Util
import Style.Parser

data BackgroundS = BackgroundS
  { _visibility :: Visibility -- defaults to visible
  , _backgroundOpacity :: IsoExpr INum -- defaults to 1, need to support inrepolate expressions
  , _backgroundColor :: IsoExpr Color -- defaults to #000000, disabled by fill-pattern
  -- , _backgroundPattern
  }
  deriving (Show)

makeLenses ''BackgroundS

instance A.FromJSON BackgroundS where
  parseJSON = A.withObject "BackgroundS" $ \t ->
    BackgroundS
      <$> t A..:? "visibility" A..!= Visible
      <*> (t A..:? "background-opacity" >>= expr) A..!= NumE 1
      <*> (t A..:? "background-color" >>= color) A..!= ColorE (black `withOpacity` 1)

-- background-pattern
