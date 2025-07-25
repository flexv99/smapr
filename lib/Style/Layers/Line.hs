{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Style.Layers.Line where

import qualified Data.Aeson as A
import Data.Colour
import Data.Text (toLower)
import qualified Diagrams.Attributes as D
import Lens.Micro
import Lens.Micro.TH
import Style.Lang.Ast
import Style.Lang.Parser
import Style.Lang.Types
import Style.Layers.Util

-- - Butt: A cap with a squared-off end which is drawn to the exact endpoint of the line.
-- - CRound: A cap with a rounded end which is drawn beyond the endpoint of the line at a radius of one-half of the line's width and centered on the endpoint of the line.
-- - Square: A cap with a squared-off end which is drawn beyond the endpoint of the line at a distance of one-half of the line's width.
-- defaults to Butt
instance A.FromJSON D.LineCap where
  parseJSON = A.withText "LineCap" $ \t -> case toLower t of
    "butt" -> return D.LineCapButt
    "round" -> return D.LineCapRound
    "square" -> return D.LineCapSquare
    _ -> return D.LineCapButt

-- - Bevel: A join with a squared-off end which is drawn beyond the endpoint of the line at a distance of one-half of the line's width.
-- - JRound: A join with a rounded end which is drawn beyond the endpoint of the line at a radius of one-half of the line's width and centered on the endpoint of the line.
-- - Miter: A join with a sharp, angled corner which is drawn with the outer sides beyond the endpoint of the path until they meet.
-- optional

instance A.FromJSON D.LineJoin where
  parseJSON = A.withText "LineJoin" $ \t -> case toLower t of
    "bevel" -> return D.LineJoinBevel
    "round" -> return D.LineJoinRound
    "miter" -> return D.LineJoinMiter
    _ -> error "[Fatal] invalid line-join enum type"

newtype ResolvedImage = ResolvedImage
  {iconImage :: [[String]]}
  deriving (Show, Eq)

instance A.FromJSON ResolvedImage where
  parseJSON = A.withObject "ResolvedImage" $ \v -> ResolvedImage <$> v A..: "icon-image"

data LineS = LineS
  { _lineCap :: D.LineCap
  , _lineJoin :: D.LineJoin
  , _lineMiterLimit :: SExpr SNum -- defaults to 2, interpolate support
  , _lineRoundLimit :: SExpr SNum -- defaults to 1.05, interpolate support
  , _lineSortKey :: Maybe (SExpr SNum)
  , _visibility :: Visibility -- defaults to Visible
  , _lineOpacity :: SExpr SNum -- defaults to 1, interpolate support
  , _lineColor :: SExpr SColor -- defaults to #000000, TODO interpolate support
  -- _lineTranslate :: WrappedExpr, -- defaults to [0, 0]
  , _lineTranslateAnchor :: TranslateAnchor -- defaults to Map
  , _lineWidth :: SExpr SNum -- defaults to 1
  , _lineGapWidth :: SExpr SNum -- defaults to 0
  , _lineOffset :: SExpr SNum -- defaults to 0
  , _lineBlur :: SExpr SNum -- defaults to 0
  -- _lineDasharray :: Maybe WrappedExpr,
  , _linePattern :: Maybe ResolvedImage
  -- _lineGradient :: Maybe WrappedExpr
  }
  deriving (Show)

makeLenses ''LineS

instance A.FromJSON LineS where
  parseJSON = A.withObject "LineS" $ \t ->
    LineS
      <$> t A..:? "line-cap" A..!= D.LineCapButt
      <*> t A..:? "line-join" A..!= D.LineJoinMiter
      <*> (t A..:? "line-miter-limit" >>= expr) A..!= NumE (Just 2)
      <*> (t A..:? "line-round-limit" >>= expr) A..!= NumE (Just 1.05)
      <*> (t A..:? "line-sort-key" >>= expr)
      <*> t A..:? "visibility" A..!= Visible
      <*> (t A..:? "line-opacity" >>= expr) A..!= NumE (Just 1)
      <*> (t A..:? "line-color" >>= color)
        A..!= ColorE
          (Just (black `withOpacity` 1))
      -- <*> (t A..:? "line-translate" >>= expr) A..!= wrap (IsoArg $ ArrayE $ SArray [SNum $ SInt 0, SNum $ SInt 0])
      <*> t A..:? "line-translate-anchor" A..!= Map
      <*> (t A..:? "line-width" >>= expr) A..!= NumE (Just 1.0)
      <*> (t A..:? "line-gap-width" >>= expr) A..!= NumE (Just 0.0)
      <*> (t A..:? "line-offset" >>= expr) A..!= NumE (Just 0.0)
      <*> (t A..:? "line-blur" >>= expr) A..!= NumE (Just 0.0)
      -- <*> (t A..:? "line-dash-array" >>= expr)
      <*> t A..:? "line-pattern"

-- <*> (t A..:? "line-gradient" >>= expr)
