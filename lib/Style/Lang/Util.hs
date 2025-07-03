module Style.Lang.Util (
  Color,
  hslToColor,
  rgbToColor,
  expandShortHex,
  colorFromHexDigits,
  pureColor,
  showSColor,
)
where

import Data.Colour
import Data.Colour.RGBSpace.HSL
import Data.Colour.SRGB
import GHC.Word

--------------------------------------------------------------------------------
-- Color Utility
--------------------------------------------------------------------------------

type Color = AlphaColour Double

hslToColor :: Double -> Double -> Double -> Double -> Color
hslToColor h s l o = sRGB (channelRed rgb) (channelGreen rgb) (channelBlue rgb) `withOpacity` o
  where
    rgb = hsl h (s / 100) (l / 100)

rgbToColor :: Word8 -> Word8 -> Word8 -> Double -> Color
rgbToColor r g b o = sRGB24 r g b `withOpacity` o

expandShortHex :: String -> String
expandShortHex hex
  | length hex == 3 = concatMap (replicate 2) hex
  | otherwise = hex

colorFromHexDigits :: (RealFloat a) => String -> AlphaColour a
colorFromHexDigits d = sRGB24read ("#" <> expandShortHex d) `withOpacity` 1

pureColor :: (Ord a, Fractional a) => AlphaColour a -> Colour a
pureColor ac
  | alphaChannel ac > 0 = darken (recip $ alphaChannel ac) (ac `over` black)
  | otherwise = error "transparent has no pure colour"

showSColor :: Color -> String
showSColor a = sRGB24show $ pureColor a
