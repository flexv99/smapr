{-# LANGUAGE TemplateHaskell #-}

module Style.ExpressionsContext where

import Control.Lens
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

featureGeometryType :: ExpressionContext -> Maybe Tile'GeomType
featureGeometryType ctx = undefined -- type' ^. (ctx ^. feature)
