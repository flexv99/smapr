{-# LANGUAGE TemplateHaskell #-}

module Style.ExpressionsContext where

import qualified Data.Map as MP
import qualified Data.Text.Lazy as T
import Control.Lens
import Data.Maybe
import Style.Parser
import Proto.Vector_tile.Tile
import Proto.Vector_tile.Tile.Layer
import Proto.Vector_tile.Tile.Feature
import Proto.Vector_tile.Tile.GeomType

data ExpressionContext = ExpressionContext
  { _feature :: Feature
  , _layer   :: Layer
  , _zoom    :: Int
  } deriving (Show, Eq)

makeLenses ''ExpressionContext

featureGeometryType :: ExpressionContext -> Maybe GeomType
featureGeometryType ctx = type' (ctx ^. feature)
