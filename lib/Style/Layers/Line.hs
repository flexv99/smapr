{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Style.Layers.Line where

import GHC.Enum
import qualified Data.Aeson as A
import Control.Lens

-- - Butt: A cap with a squared-off end which is drawn to the exact endpoint of the line.
-- - CRound: A cap with a rounded end which is drawn beyond the endpoint of the line at a radius of one-half of the line's width and centered on the endpoint of the line.
-- - Square: A cap with a squared-off end which is drawn beyond the endpoint of the line at a distance of one-half of the line's width.
-- defaults to Butt
data LineCap = Butt | CRound | Square deriving (Enum, Eq, Show)


-- - Bevel: A join with a squared-off end which is drawn beyond the endpoint of the line at a distance of one-half of the line's width.
-- - JRound: A join with a rounded end which is drawn beyond the endpoint of the line at a radius of one-half of the line's width and centered on the endpoint of the line.
-- - Miter: A join with a sharp, angled corner which is drawn with the outer sides beyond the endpoint of the path until they meet.
-- optional
data LineJoin = Bevel | JRound | Miter deriving (Enum, Eq, Show)

-- - Visible: The layer is shown.
-- - None: The layer is not shown.
-- defaults to Visible
data Visibility = Visible | None deriving (Enum, Eq, Show)

-- - Map: The line is translated relative to the map.
-- - Viewport: The line is translated relative to the viewport.
-- defaults to Map
data LineTranslateAnchor = Map | Viewport deriving (Enum, Eq, Show)

data ResolvedImage where
  ResolvedImage :: {iconImage :: [[String]]} -> ResolvedImage

instance Show ResolvedImage where
  show _ = "img"

instance Eq ResolvedImage where
  (==) (ResolvedImage a) (ResolvedImage b) = a == b

data LineS = LineS 
  { lineCap :: Maybe LineCap
  , _lineJoin :: Maybe LineJoin
  , _lineMiterLimit :: Maybe Int -- defaults to 2
  , _lineRoundLimit :: Maybe Double -- defaults to 1.05
  , _lineSortKey :: Maybe Int
  , _visibility :: Maybe Visibility
  , _lineOpacity :: Maybe Double -- defaults to 1
  , _lineColor :: Maybe String
  , _lineTranslate :: Maybe [Int] -- defaults to [0, 0]
  , _lineTranslateAnchor :: Maybe LineTranslateAnchor
  , _lineWidth :: Maybe Double -- defaults to 1
  , _lineGapWidth :: Maybe Double -- defaults to 0
  , _lineOffset :: Maybe Double -- defaults to 0
  , _lineBlur :: Maybe Double -- defaults to 0
  , _lineDasharray :: Maybe [Double]
  , _linePattern :: Maybe ResolvedImage
  , _lineGradient :: Maybe String
  } deriving (Show, Eq)
makeLenses ''LineS
