{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE RankNTypes        #-}

module Style.Layers.Line where

import Data.Text (toLower)
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Aeson.Text as A
import GHC.Enum
import Control.Lens
import Text.Megaparsec
import Style.Parser
import Style.ExpressionsWrapper
import Style.IsoExpressions

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
  { _lineCap             :: Maybe LineCap
  , _lineJoin            :: Maybe LineJoin
  , _lineMiterLimit      :: WrappedExpr             -- defaults to 2
  , _lineRoundLimit      :: WrappedExpr             -- defaults to 1.05
  , _lineSortKey         :: Maybe WrappedExpr
  , _visibility          :: Visibility      -- defaults to Visible
  , _lineOpacity         :: WrappedExpr          -- defaults to 1
  , _lineColor           :: Maybe String    -- defaults to #000000
  , _lineTranslate       :: Maybe [Int]     -- defaults to [0, 0]
  , _lineTranslateAnchor :: Maybe LineTranslateAnchor
  , _lineWidth           :: WrappedExpr          -- defaults to 1
  , _lineGapWidth        :: Double          -- defaults to 0
  , _lineOffset          :: Double          -- defaults to 0
  , _lineBlur            :: Double          -- defaults to 0
  , _lineDasharray       :: Maybe [Double]
  , _linePattern         :: Maybe ResolvedImage
  , _lineGradient        :: Maybe String
  } deriving (Show)
makeLenses ''LineS

instance A.FromJSON LineS where
  parseJSON = A.withObject "LineS" $ \t -> LineS
    <$> t A..:? "line-cap"
    <*> t A..:? "line-join"
    <*> (t A..:? "line-miter-limit" >>= expr) A..!= wrap (IsoArg $ IntE 2)
    <*> (t A..:? "line-round-limit" >>= expr) A..!= wrap (IsoArg $ DoubleE 1.05)
    <*> (t A..:? "line-sort-key" >>= expr)
    <*> t A..:? "visibility" A..!= Visible
    <*> (t A..:? "line-opacity" >>= expr) A..!= wrap (IsoArg $ IntE 1)
    <*> t A..:? "line-color"
    <*> t A..:? "line-translate"
    <*> t A..:? "line-translate-anchor"
    <*> (t A..:? "line-width" >>= expr) A..!= wrap (IsoArg $ DoubleE 1.0)
    <*> t A..:? "line-gap-width" A..!= 0
    <*> t A..:? "line-offset" A..!= 0
    <*> t A..:? "line-blur" A..!= 0
    <*> t A..:? "line-dash-array"
    <*> t A..:? "line-pattern"
    <*> t A..:? "line-gradient"
    where
      expr :: Maybe A.Value -> A.Parser (Maybe WrappedExpr)
      expr  (Just v) = case parse (try interpolateP <|> numRetExprP) "" (A.encodeToLazyText v) of
                                   Left err  -> fail $ errorBundlePretty err
                                   Right res -> pure $ Just $ wrap res
      expr Nothing   = pure Nothing
