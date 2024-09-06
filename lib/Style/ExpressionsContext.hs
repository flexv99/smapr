{-# LANGUAGE TemplateHaskell #-}

module Style.ExpressionsContext where

import Control.Lens
import Proto.Vector_tile.Tile.Layer
import Proto.Vector_tile.Tile.Feature
import Proto.Vector_tile.Tile.GeomType

data ExpressionContext = ExpressionContext
  { _feature :: Feature
  , _layer   :: Layer
  , _ctxZoom    :: Int
  } deriving (Show, Eq)

makeLenses ''ExpressionContext

featureGeometryType :: ExpressionContext -> Maybe GeomType
featureGeometryType ctx = type' (ctx ^. feature)
