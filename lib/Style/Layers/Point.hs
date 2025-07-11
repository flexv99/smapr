{-# LANGUAGE TemplateHaskell #-}

module Style.Layers.Point where

import qualified Data.Aeson as A
import Data.Colour (black, withOpacity)
import Data.Text (toLower)
import Lens.Micro
import Lens.Micro.TH
import Style.Lang.Ast
import Style.Lang.Parser
import Style.Lang.Types
import Style.Layers.Util

data SymbolPlacement = Point | Line | LineCenter deriving (Show)

instance A.FromJSON SymbolPlacement where
  parseJSON = A.withText "SymbolPlacement" $ \t -> case toLower t of
    "point" -> pure Point
    "line" -> pure Line
    "line-center" -> pure LineCenter
    _ -> pure Point

data SymbolZOrder = Auto | ViewPortY | Source deriving (Show)

instance A.FromJSON SymbolZOrder where
  parseJSON = A.withText "SymbolZOrder" $ \t -> case toLower t of
    "auto" -> pure Auto
    "viewport-y" -> pure ViewPortY
    "source" -> pure Source
    _ -> pure Auto

data PointS = PointS
  { _symbolPlacement :: SExpr SString -- DEF: Point
  , _symbolSpacing :: SExpr SNum -- DEF: 250
  , _symbolAvoidEdges :: SExpr SBool -- DEF: false
  , _symbolSortKey :: Maybe (SExpr SNum)
  , _symbolZOrder :: SymbolZOrder -- DEF: Auto
  , _iconAllowOverlap :: SExpr SBool -- DEF: false
  , _textField :: SExpr SString
  , _textSize :: SExpr SNum
  , _textRotate :: SExpr SNum
  , _textColor :: SExpr SColor
  }
  deriving (Show)

makeLenses ''PointS

instance A.FromJSON PointS where
  parseJSON = A.withObject "PointS" $ \t ->
    PointS
      <$> (t A..:? "symbol-placement" >>= sexpr) A..!= StringE (Just "point")
      <*> (t A..:? "symbol-spacing" >>= expr) A..!= NumE (Just 250)
      <*> (t A..:? "symbol-avoid-edges" >>= bexpr) A..!= BoolE (Just False)
      <*> (t A..:? "symbol-sort-key" >>= expr)
      <*> t A..:? "symbol-z-order" A..!= Auto
      <*> (t A..:? "icon-allow-overlap" >>= bexpr) A..!= BoolE (Just False)
      <*> (t A..:? "text-field" >>= sexpr) A..!= StringE (Just "")
      <*> (t A..:? "text-size" >>= expr) A..!= NumE (Just 16)
      <*> (t A..:? "text-rotate" >>= expr) A..!= NumE (Just 0)
      <*> (t A..:? "text-color" >>= color) A..!= ColorE (Just $ black `withOpacity` 1)

{-

icon-overlap
icon-ignore-placement
icon-optional
icon-rotation-alignment
icon-size
icon-text-fit
icon-text-fit-padding
icon-image
icon-rotate
icon-padding
icon-keep-upright
icon-offset
icon-anchor
icon-pitch-alignment
text-pitch-alignment
text-rotation-alignment

text-font

text-max-width
text-line-height
text-letter-spacing
text-justify
text-radial-offset
text-variable-anchor
text-variable-anchor-offset
text-anchor
text-max-angle
text-writing-mode

text-padding
text-keep-upright
text-transform
text-offset
text-allow-overlap
text-overlap
text-ignore-placement
text-optional
visibility
icon-opacity
icon-color
icon-halo-color
icon-halo-width
icon-halo-blur
icon-translate
icon-translate-anchor
text-opacity

text-halo-color
text-halo-width
text-halo-blur
text-translate
text-translate-anchor-}
