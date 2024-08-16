{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Style.Layers.Line where

import GHC.Enum
import qualified Data.Aeson as A
import Control.Lens
import Data.Text (toLower)

-- - Butt: A cap with a squared-off end which is drawn to the exact endpoint of the line.
-- - CRound: A cap with a rounded end which is drawn beyond the endpoint of the line at a radius of one-half of the line's width and centered on the endpoint of the line.
-- - Square: A cap with a squared-off end which is drawn beyond the endpoint of the line at a distance of one-half of the line's width.
-- defaults to Butt
data LineCap = Butt | CRound | Square deriving (Enum, Eq, Show)

instance A.FromJSON LineCap where
  parseJSON = A.withText "LineCap" $ \t -> case toLower t of
    "butt"   -> return Butt
    "round"  -> return CRound
    "square" -> return Square
    _        -> return Butt

-- - Bevel: A join with a squared-off end which is drawn beyond the endpoint of the line at a distance of one-half of the line's width.
-- - JRound: A join with a rounded end which is drawn beyond the endpoint of the line at a radius of one-half of the line's width and centered on the endpoint of the line.
-- - Miter: A join with a sharp, angled corner which is drawn with the outer sides beyond the endpoint of the path until they meet.
-- optional
data LineJoin = Bevel | JRound | Miter deriving (Enum, Eq, Show)

instance A.FromJSON LineJoin where
  parseJSON = A.withText "LineJoin" $ \t -> case toLower t of
    "bevel" -> return Bevel
    "round" -> return JRound
    "miter" -> return Miter
    _       -> error "[Fatal] invalid line-join enum type"

-- - Visible: The layer is shown.
-- - None: The layer is not shown.
-- defaults to Visible
data Visibility = Visible | None deriving (Enum, Eq, Show)

instance A.FromJSON Visibility where
  parseJSON = A.withText "Visibility" $ \t -> case toLower t of
    "visible" -> return Visible
    "none"    -> return None
    _         -> return Visible

-- - Map: The line is translated relative to the map.
-- - Viewport: The line is translated relative to the viewport.
-- defaults to Map
data LineTranslateAnchor = Map | Viewport deriving (Enum, Eq, Show)

instance A.FromJSON LineTranslateAnchor where
  parseJSON = A.withText "LineTranslateAnchor" $ \t -> case toLower t of
    "map"      -> return Map
    "viewport" -> return Viewport
    _          -> return Map 

newtype ResolvedImage = ResolvedImage
  { iconImage :: [[String]] } deriving (Show, Eq)

instance A.FromJSON ResolvedImage where
  parseJSON = A.withObject "ResolvedImage" $ \v -> ResolvedImage <$> v A..: "icon-image"

data LineS = LineS 
  { _lineCap :: Maybe LineCap
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

instance A.FromJSON LineS where
  parseJSON = A.withObject "LineS" $ \t -> LineS
    <$> t A..:? "line-cap"
    <*> t A..:? "line-join"
    <*> t A..:? "line-miter-limit"
    <*> t A..:? "line-round-limit"
    <*> t A..:? "line-sort-key"
    <*> t A..:? "visibility"
    <*> t A..:? "line-opacity"
    <*> t A..:? "line-color"
    <*> t A..:? "line-translate"
    <*> t A..:? "line-translate-anchor"
    <*> t A..:? "line-width"
    <*> t A..:? "line-gap-width"
    <*> t A..:? "line-offset"
    <*> t A..:? "line-blur"
    <*> t A..:? "line-dash-array"
    <*> t A..:? "line-pattern"
    <*> t A..:? "line-gradient"
