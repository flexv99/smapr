{-# LANGUAGE TypeOperators, DeriveGeneric #-}

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

-- Here are defined the types being used in the style spec
-- reference: https://github.com/maplibre/maplibre-style-spec/blob/main/src/expression/types.ts#L133
-- Docs: https://maplibre.org/maplibre-style-spec/expressions/#types

type Color = Colour Double


data SType
  = SInt Int
  | SDouble Double
  | SString T.Text
  | SBool Bool
  | SColor Color
  | SArray [SType]
  | SNull
  deriving (Show, Generic, Eq)

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

skipComma :: Parser a -> Parser a
skipComma = L.lexeme (skipMany (spaceChar <|> char ','))

betweenBrackets :: (Token s ~ Char, MonadParsec e s m) => m a -> m a
betweenBrackets = between (char '(' >> space) (char ')' >> space)

betweenSquareBrackets :: (Token s ~ Char, MonadParsec e s m) => m a -> m a
betweenSquareBrackets = between (char '[' >> space) (char ']' >> space)

betweenDoubleQuotes :: (Token s ~ Char, MonadParsec e s m) => m a -> m a
betweenDoubleQuotes = between (char '"' >> space) (char '"' >> space)


--- PARSER

pKeyword :: T.Text -> Parser T.Text
pKeyword keyword =
  label ("property_key: " ++ T.unpack keyword) $
    betweenDoubleQuotes $
      lexeme (string keyword <* notFollowedBy alphaNumChar)

pString :: Parser T.Text
pString = fmap
      T.pack
      (betweenDoubleQuotes
          (lexeme (many snakeCaseChar) <?> "string"))

pBool :: Parser Bool
pBool = lexeme
      (False <$ (string "false" *> notFollowedBy alphaNumChar))
        <|> (True <$ (string "true" *> notFollowedBy alphaNumChar)) <?> "bool"

pInteger :: Parser Int
pInteger = lexeme (L.signed space L.decimal) <?> "integer"

pDouble :: Parser Double
pDouble = lexeme (L.signed space L.float) <?> "float"

pAtom :: Parser SType
pAtom =
  try $
    choice
      [ numberLitP
      , boolLitP
      , nullP
      , try pHslColor -- todo needs to be generalized color parser
      , stringLitP
      , arrayLitP
      ]

parserForType :: SType -> Parser SType
parserForType t = case t of
  SInt _    -> intLitP
  SDouble _ -> doubleLitP
  SBool _   -> boolLitP
  SString _ -> stringLitP
  SArray _  -> arrayLitP
  SNull     -> nullP
  _         -> pAtom

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

intLitP :: Parser SType
intLitP = SInt <$> pInteger

doubleLitP :: Parser SType
doubleLitP = SDouble <$> pDouble

numberLitP :: Parser SType
numberLitP = try doubleLitP <|> intLitP <?> "number"

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

hslToColor :: Double -> Double -> Double  -> Color
hslToColor h s l = sRGB (channelRed rgb) (channelGreen rgb) (channelBlue rgb)
  where
    rgb = hsl h (s / 100) (l / 100)

pHslColor :: Parser SType
pHslColor = do
           _ <- lexeme (string "hsl" <* notFollowedBy alphaNumChar)
           betweenBrackets $ do
             hue        <- pInt
             _          <- char ',' >> space
             saturation <- pColorPercentage
             _          <- char ',' >> space
             SColor . hslToColor hue saturation <$> pColorPercentage
               where
                 pInt = lexeme (L.signed space L.decimal)
                 pColorPercentage = do
                   num <- pInt
                   _   <- char '%'
                   return num

showSColor :: SType -> String
showSColor (SColor a) = sRGB24show a

exprBaseP :: T.Text -> Parser a -> Parser a
exprBaseP id rest = betweenSquareBrackets $ do
  _ <- pKeyword id
  _ <- char ',' >> space
  rest
