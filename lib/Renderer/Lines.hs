module Renderer.Lines
  ( lineToPoints
  ) where

import Proto.Vector_tile.Tile.Layer (Layer(..))
import Proto.Vector_tile.Tile.Feature (Feature(..))
import Proto.Vector_tile.Tile (Tile(..))
import Data.Foldable (toList)
import qualified Data.Text.Lazy as T
import qualified Diagrams.Prelude as D
import qualified Diagrams.TwoD.Size as D
import qualified Diagrams.Backend.SVG as D
import qualified Diagrams.Trail as D
import qualified Data.Sequence as SQ
import Control.Monad
import Util
import ApiClient
import Decoder.Geometry
import Decoder.Lines
import Style.ExpressionsEval

render2DVector :: D.Diagram D.B -> IO ()
render2DVector v = do
  let sz = D.mkSizeSpec2D (Just 512) (Just 512)
  dateStr <- dateTimeStr
  path <- testPath dateStr
  putStrLn path
  D.renderSVG path sz $ v D.# D.showOrigin

geometryPointToDPoint :: Point -> D.P2 Double
geometryPointToDPoint (x, y) = x D.^& y

lineToPoints :: LineG -> [D.P2 Double]
lineToPoints (LineG lMoveTo lLineTo) = toDPoint $ _parameters lMoveTo ++ _parameters lLineTo
  where
    toDPoint = map geometryPointToDPoint

-- drawTourWithStyle :: Tile -> POCLayer -> D.Diagram D.B
-- drawTourWithStyle = undefined


{-
drawTourWithStyle tour = tourPoints <> D.strokeP tourPath
  where
    tourPath = D.fromVertices tour
    tourPoints = D.atPoints (concat . D.pathVertices $ tourPath) (repeat dot)
    dot = D.circle 0.05 D.# D.fc D.black
-}
