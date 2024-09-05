{-# LANGUAGE TemplateHaskell #-}

module Style.ExpressionsContext where

import qualified Data.Map as MP
import qualified Data.Text.Lazy as T
import Control.Lens
import Style.Parser
import Proto.Vector_tile.Tile
import Proto.Vector_tile.Tile.Layer
import Proto.Vector_tile.Tile.Feature

data ExpressionContext = ExpressionContext
  { _feature :: Feature
  , _layer   :: Layer
  , _zoom    :: Int
  } deriving (Show, Eq)

makeLenses ''ExpressionContext
