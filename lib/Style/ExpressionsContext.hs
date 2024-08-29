module Style.ExpressionsContext where

import qualified Data.Map as MP
import qualified Data.Text.Lazy as T
import Style.Parser
import Proto.Util
import Proto.Vector_tile.Tile
import Proto.Vector_tile.Tile.Layer

data ExpressionContext = ExpressionContext
  { tile :: Tile
  , zoom :: Int
  } deriving (Show, Eq)
