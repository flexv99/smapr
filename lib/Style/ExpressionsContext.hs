{-# LANGUAGE TemplateHaskell #-}

module Style.ExpressionsContext where

import Control.Lens
import Proto.Vector_tile.Tile.Feature
import Proto.Vector_tile.Tile.GeomType
import Proto.Vector_tile.Tile.Layer

data ExpressionContext = ExpressionContext
  { _feature :: Feature,
    _layer :: Layer,
    _ctxZoom :: Double
  }
  deriving (Show, Eq)

makeLenses ''ExpressionContext

featureGeometryType :: ExpressionContext -> Maybe GeomType
featureGeometryType ctx = type' (ctx ^. feature)
