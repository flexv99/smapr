{-# LANGUAGE TemplateHaskell #-}

module Style.ExpressionsContext where

import Lens.Micro
import Lens.Micro.TH
import Proto.Vector
import Proto.Vector_Fields

-- perhabs make it a:
-- properties
-- zoom
-- geometry-type
-- path
-- kind of object

data ExpressionContext = ExpressionContext
  { _feature :: Tile'Feature
  , _layer :: Tile'Layer
  , _ctxZoom :: Double
  }
  deriving (Show, Eq)

makeLenses ''ExpressionContext

featureGeometryType :: ExpressionContext -> Tile'GeomType
featureGeometryType ctx = ctx ^. (feature . type')
