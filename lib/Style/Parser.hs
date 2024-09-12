{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Style.Parser where

import qualified Data.Aeson as A
import Data.Colour
import Data.Colour.RGBSpace.HSL
import Data.Colour.SRGB
import Data.List (singleton)
import Data.Scientific (isFloating, toRealFloat)
import qualified Data.Text.Internal.Lazy as T
import qualified Data.Text.Lazy as T
import qualified Data.Vector as V
import Data.Void
import GHC.Generics (Generic)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- Void: The type for custom error messages. We have none, so use `Void`.
-- T.Text: The input stream type.
type Parser = Parsec Void T.Text

-- Here are defined the types being used in the style spec
-- reference: https://github.com/maplibre/maplibre-style-spec/blob/main/src/expression/types.ts#L133
-- Docs: https://maplibre.org/maplibre-style-spec/expressions/#types

type Color = AlphaColour Double

data INum = SInt Int | SDouble Double deriving (Show, Generic, Eq, Ord)

data SType
  = SNum INum
  | SString T.Text
  | SBool Bool
  | SColor Color
  | SArray [SType]
  | SNull
  deriving (Show, Generic, Eq)

instance A.FromJSON SType where
  parseJSON (A.Number n) =
    if isFloating n
      then pure $ SNum $ SDouble (toRealFloat n)
      else pure $ SNum $ SInt (round n)
  parseJSON (A.Bool b) = pure $ SBool b
  parseJSON (A.Array a) = SArray <$> traverse A.parseJSON (V.toList a)
  parseJSON a =
    A.withText
      "SType"
      ( \v ->
          case parse pAtom "" (T.fromStrict v) of
            Left err -> fail $ errorBundlePretty err
            Right res -> return res
      )
      a

--- HELPERS

exprBaseP :: T.Text -> Parser a -> Parser a
exprBaseP i rest = betweenSquareBrackets $ do
  _ <- pKeyword i
  _ <- char ',' >> space
  rest

-- the space consumer
sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: T.Text -> Parser T.Text
symbol = L.symbol sc

-- parse snake case property names
snakeCaseChar :: Parser Char
snakeCaseChar = alphaNumChar <|> char '_'

skipComma :: Parser a -> Parser a
skipComma = L.lexeme (skipMany (spaceChar <|> char ','))

betweenBrackets :: (Token s ~ Char, MonadParsec e s m) => m a -> m a
betweenBrackets = between (char '(' >> space) (char ')' >> space)

betweenSquareBrackets :: (Token s ~ Char, MonadParsec e s m) => m a -> m a
betweenSquareBrackets = between (char '[' >> space) (char ']' >> space)

betweenDoubleQuotes :: (Token s ~ Char, MonadParsec e s m) => m a -> m a
betweenDoubleQuotes = between (char '"' >> space) (char '"' >> space)

numToDouble :: INum -> Double
numToDouble (SDouble d) = d
numToDouble (SInt i) = fromIntegral i

--- PARSER

pKeyword :: T.Text -> Parser T.Text
pKeyword keyword =
  label ("property_key: " ++ T.unpack keyword) $
    betweenDoubleQuotes $
      lexeme (string keyword <* notFollowedBy alphaNumChar)

pString :: Parser T.Text
pString =
  fmap
    T.pack
    ( betweenDoubleQuotes
        (lexeme (many snakeCaseChar) <?> "string")
    )

pBool :: Parser Bool
pBool =
  lexeme
    (False <$ (string "false" *> notFollowedBy alphaNumChar))
    <|> (True <$ (string "true" *> notFollowedBy alphaNumChar))
    <?> "bool"

pInteger :: Parser Int
pInteger = lexeme (L.signed space L.decimal) <?> "integer"

pDouble :: Parser Double
pDouble = lexeme (L.signed space L.float) <?> "float"

pColor :: Parser SType
pColor = choice $ map try [pHslColor, pRgbaColor, pRgbColor, pHexColor]

pAtom :: Parser SType
pAtom =
  try $
    choice
      [ numberLitP,
        boolLitP,
        nullP,
        try pColor,
        stringLitP,
        arrayLitP
      ]

parserForType :: SType -> Parser SType
parserForType t = case t of
  SNum a -> numP a
  SBool _ -> boolLitP
  SString _ -> stringLitP
  SArray _ -> arrayLitP
  SNull -> nullP
  _ -> pAtom
  where
    numP :: INum -> Parser SType
    numP (SInt _) = SNum <$> intLitP
    numP (SDouble _) = SNum <$> doubleLitP

pArray :: Parser [SType]
pArray =
  label "array" $ betweenSquareBrackets $ do
    firstElem <- pAtom
    _ <- char ',' >> space
    restElems <- parserForType firstElem `sepBy` (char ',' >> space)
    return (firstElem : restElems)

stringLitP :: Parser SType
stringLitP = SString <$> pString

boolLitP :: Parser SType
boolLitP = SBool <$> pBool

intLitP :: Parser INum
intLitP = SInt <$> pInteger

doubleLitP :: Parser INum
doubleLitP = SDouble <$> pDouble

numberLitINumP :: Parser INum
numberLitINumP = (try doubleLitP <|> intLitP) <?> "number"

numberLitP :: Parser SType
numberLitP = SNum <$> numberLitINumP

arrayLitP :: Parser SType
arrayLitP = SArray <$> pArray

nullP :: Parser SType
nullP = lexeme (SNull <$ (string "null" *> notFollowedBy alphaNumChar)) <?> "null"

-- Color
-- The color type is a color in the sRGB color space. Colors are JSON strings in a variety of permitted formats: HTML-style hex values, RGB, RGBA, HSL, and HSLA. Predefined HTML colors names, like yellow and blue, are also permitted.
-- {
--     "line-color": "#ff0",
--     "line-color": "#ffff00",
--     "line-color": "rgb(255, 255, 0)",
--     "line-color": "rgba(255, 255, 0, 1)",
--     "line-color": "hsl(100, 50%, 50%)",
--     "line-color": "hsla(100, 50%, 50%, 1)",
--     "line-color": "yellow"
-- }

pHslColor :: Parser SType
pHslColor = betweenDoubleQuotes $ do
  _ <- lexeme (string "hsl" <* notFollowedBy alphaNumChar)
  betweenBrackets $ do
    h <- pInt
    _ <- char ',' >> space
    s <- pColorPercentage
    _ <- char ',' >> space
    SColor . hslToColor h s <$> pColorPercentage
  where
    pInt = lexeme (L.signed space L.decimal)
    pColorPercentage = do
      num <- pInt
      _ <- char '%'
      return num

pRgbColor :: Parser SType
pRgbColor = betweenDoubleQuotes $ do
  _ <- lexeme (string "rgb" <* notFollowedBy alphaNumChar)
  betweenBrackets $ do
    r <- fromIntegral <$> pInteger
    _ <- char ',' >> space
    g <- fromIntegral <$> pInteger
    _ <- char ',' >> space
    b <- fromIntegral <$> pInteger
    return $ SColor $ sRGB24 r g b `withOpacity` 1

pRgbaColor :: Parser SType
pRgbaColor = betweenDoubleQuotes $ do
  _ <- lexeme (string "rgba" <* notFollowedBy alphaNumChar)
  betweenBrackets $ do
    r <- fromIntegral <$> pInteger
    _ <- char ',' >> space
    g <- fromIntegral <$> pInteger
    _ <- char ',' >> space
    b <- fromIntegral <$> pInteger
    _ <- char ',' >> space
    opacity <- numberLitINumP
    return $ SColor $ sRGB24 r g b `withOpacity` numToDouble opacity

expandShortHex :: String -> String
expandShortHex hex
  | length hex == 3 = concatMap (replicate 2 . head . singleton) hex
  | otherwise = hex

pHexColor :: Parser SType
pHexColor = betweenDoubleQuotes $ do
  _ <- char '#'
  hexDigits <- try (some hexDigitChar)
  let validLength = length hexDigits == 6 || length hexDigits == 3
  if validLength
    then return $ SColor $ sRGB24read ("#" <> expandShortHex hexDigits) `withOpacity` 1
    else fail "Invalid hex color code length"

hslToColor :: Double -> Double -> Double -> Color
hslToColor h s l = opaque $ sRGB (channelRed rgb) (channelGreen rgb) (channelBlue rgb)
  where
    rgb = hsl h (s / 100) (l / 100)

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

showSColor :: SType -> String
showSColor (SColor a) = sRGB24show $ pureColour a
  where
    pureColour ac
      | alphaChannel ac > 0 = darken (recip $ alphaChannel ac) (ac `over` black)
      | otherwise = error "transparent has no pure colour"
showSColor _ = error "can only show colors"

tuplifyWithFallback :: [a] -> ([(a, a)], a)
tuplifyWithFallback [] = error "no fallback value"
tuplifyWithFallback [x] = ([], x)
tuplifyWithFallback (x : y : xs) =
  let (tuples, lastElem) = tuplifyWithFallback xs
   in ((x, y) : tuples, lastElem)
