module Style.Lang.Util
  ( Color (..),
    hslToColor,
    rgbToColor,
    expandShortHex,
    colorFromHexDigits,
  )
where

import Data.Colour
import Data.Colour.RGBSpace.HSL
import Data.Colour.SRGB
import Data.List
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
  | length hex == 3 = concatMap (replicate 2 . head . singleton) hex
  | otherwise = hex

colorFromHexDigits :: (RealFloat a) => String -> AlphaColour a
colorFromHexDigits d = sRGB24read ("#" <> expandShortHex d) `withOpacity` 1
