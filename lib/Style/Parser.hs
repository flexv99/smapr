{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Style.Parser where

import qualified Data.Aeson.Types as A
import Data.Colour
import Data.Colour.RGBSpace.HSL
import Data.Colour.SRGB
import qualified Data.Text.Internal.Lazy as T
import qualified Data.Text.Lazy as T
import Data.Void
import GHC.Generics (Generic)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- Void: The type for custom error messages. We have none, so use `Void`.
-- T.Text: The input stream type.
type Parser = Parsec Void T.Text

newtype StylesArray = StylesArray
  {getArray :: ([SType], Int, String)}
  deriving (Show, Generic)

-- Here are defined the types being used in the style spec
-- reference: https://github.com/maplibre/maplibre-style-spec/blob/main/src/expression/types.ts#L133
-- Docs: https://maplibre.org/maplibre-style-spec/expressions/#types

{-
Supported datatypes:
missing ones:
'null'
'color'
'object'
'value'
'error'
'collator'
'formatted'
'padding'
'resolvedImage'
'variableAnchorOffsetCollection'
-}

data SType
  = SInteger Int
  | SDouble Double
  | SString T.Text
  | SBool Bool
  | SColor Color
  | SArray [SType]
  deriving (-- | STypeOf  String
            -- | SArray   StylesArray
            Show, Generic, Eq)

--- HELPERS

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

betweenBrackets :: (Token s ~ Char, MonadParsec e s m) => m a -> m a
betweenBrackets = between (char '(' >> space) (char ')' >> space)

betweenSquareBrackets :: (Token s ~ Char, MonadParsec e s m) => m a -> m a
betweenSquareBrackets = between (char '[' >> space) (char ']' >> space)

betweenDoubleQuotes :: (Token s ~ Char, MonadParsec e s m) => m a -> m a
betweenDoubleQuotes = between (char '"' >> space) (char '"' >> space)

--- PARSER

pString :: Parser SType
pString =
 SString
    <$> fmap
      T.pack
      (betweenDoubleQuotes
          (lexeme (many snakeCaseChar) <?> "string literal"))

pInteger :: Parser SType
pInteger = SInteger <$> lexeme (L.signed space L.decimal)

pDouble :: Parser SType
pDouble = SDouble <$> lexeme (L.signed space L.float)

pNumber :: Parser SType
pNumber = try pDouble <|> pInteger <?> "number"

pBool :: Parser SType
pBool =
  label "bool" $
    lexeme $
      (SBool False <$ (string "false" *> notFollowedBy alphaNumChar))
        <|> (SBool True <$ (string "true" *> notFollowedBy alphaNumChar))
                   

pAtom :: Parser SType
pAtom =
  try $
    choice
      [
        pNumber,
        pBool,
        pHslColor,
        pString
      ]

pArray :: Parser [SType]
pArray =
  betweenSquareBrackets
    (pAtom `sepBy` (char ',' >> space))

skipComma :: Parser a -> Parser a
skipComma = L.lexeme (skipMany (spaceChar <|> char ','))

pKeyword :: T.Text -> Parser T.Text
pKeyword keyword =
  label ("property_key: " ++ T.unpack keyword) $
    betweenDoubleQuotes $
      lexeme (string keyword <* notFollowedBy alphaNumChar)

literalId :: T.Text
literalId = "literal"

literal :: Parser [SType]
literal = label (show literalId) $ betweenSquareBrackets $ do
  key <- pKeyword literalId
  _ <- char ',' >> space
  pArray


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

type Color = Colour Double


hslToColor :: Double -> Double -> Double  -> Color
hslToColor h s l = sRGB (channelRed rgb) (channelGreen rgb) (channelBlue rgb)
  where
    rgb = hsl h (s / 100) (l / 100)

pHslColor :: Parser SType
pHslColor = do
           key <- lexeme (string "hsl" <* notFollowedBy alphaNumChar)
           betweenBrackets $ do
             hue        <- pInt
             _          <- char ',' >> space
             saturation <- pColorPercentage
             _          <- char ',' >> space
             lightness  <- pColorPercentage
             pure $ SColor (hslToColor hue saturation lightness)
               where
                 pInt = lexeme (L.signed space L.decimal)
                 pColorPercentage = do
                   num <- pInt
                   _   <- char '%'
                   return num

showSColor :: SType -> String
showSColor (SColor a) = sRGB24show a 
