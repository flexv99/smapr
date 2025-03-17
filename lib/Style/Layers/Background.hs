{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Style.Layers.Background where

import Control.Lens
import qualified Data.Aeson as A
import Data.Colour
import Style.Lang.Ast
import Style.Lang.Parser
import Style.Lang.Types
import Style.Layers.Util

data BackgroundS = BackgroundS
  { _visibility :: Visibility -- defaults to visible
  , _backgroundOpacity :: SExpr SNum -- defaults to 1, need to support inrepolate expressions
  , _backgroundColor :: SExpr SColor -- defaults to #000000, disabled by fill-pattern
  -- , _backgroundPattern
  }
  deriving (Show)

makeLenses ''BackgroundS

instance A.FromJSON BackgroundS where
  parseJSON = A.withObject "BackgroundS" $ \t ->
    BackgroundS
      <$> t A..:? "visibility" A..!= Visible
      <*> (t A..:? "background-opacity" >>= expr) A..!= NumE (Just 1)
      <*> (t A..:? "background-color" >>= color) A..!= ColorE (Just (black `withOpacity` 1))

-- background-pattern
