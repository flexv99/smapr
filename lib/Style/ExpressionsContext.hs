{-# LANGUAGE TemplateHaskell #-}

module Style.ExpressionsContext where

import Control.Lens
import Proto.Vector_tile.Tile.Feature
import Proto.Vector_tile.Tile.GeomType
import Proto.Vector_tile.Tile.Layer

-- perhabs make it a:
-- properties
-- zoom
-- geometry-type
-- path
-- kind of object

data ExpressionContext = ExpressionContext
  { _feature :: Feature,
    _layer :: Layer,
    _ctxZoom :: Double
  }
  deriving (Show, Eq)

makeLenses ''ExpressionContext

featureGeometryType :: ExpressionContext -> Maybe GeomType
featureGeometryType ctx = type' (ctx ^. feature)
