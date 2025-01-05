{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Style.Lang.Lex
  ( Parser (..),
    betweenBrackets,
    betweenSquareBrackets,
    betweenDoubleQuotes,
    numSymbol,
    stringSymbol,
    boolSymbol,
    colorSymbol,
    polySymbol,
    pAtom,
    pString,
    pNum,
    pBool,
    pColor,
    pArray,
    pTraversable,
  )
where

import qualified Data.Text.Lazy as T
import Data.Void
import Style.Lang.Token
import Style.Lang.Types
import Style.Lang.Util
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void T.Text

-- | space conusumer
sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

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

nullableP :: Parser a -> Parser (Maybe a)
nullableP p = Nothing <$ string "null" <|> Just <$> p

pString :: Parser SString
pString =
  nullableP
    ( T.pack
        <$> betweenDoubleQuotes
          (lexeme (many snakeCaseChar))
        <?> "string"
    )

pBool :: Parser SBool
pBool =
  nullableP
    ( lexeme
        (False <$ (string "false" *> notFollowedBy alphaNumChar))
        <|> (True <$ (string "true" *> notFollowedBy alphaNumChar))
    )
    <?> "bool"

pNum :: Parser SNum
pNum =
  nullableP
    ( lexeme (L.signed sc L.scientific)
        <|> L.scientific
    )
    <?> "number"

pColor :: Parser SColor
pColor = nullableP $ do
  pHexColor <|> (pFColor =<< colorSymbol) <?> "color"

pAtom :: Parser SData
pAtom =
  choice
    [ DNum <$> pNum,
      DBool <$> pBool,
      try $ DColor <$> pColor,
      DString <$> pString
    ]

pPercentage :: Parser Double
pPercentage = do
  num <- lexeme L.decimal
  _ <- char '%'
  return num

pDouble :: Parser Double
pDouble = lexeme L.float

pInt :: Parser Int
pInt = lexeme L.decimal

-- TODO accept lists of one datatype only
pArray :: Parser [SData]
pArray = betweenSquareBrackets $ pAtom `sepBy` (char ',' >> space)

pTraversable :: Parser (Either [SData] SString)
pTraversable = (Left <$> pArray) <|> (Right <$> pString)

--------------------------------------------------------------------------------

colorSymbol :: Parser ColorToken
colorSymbol =
  choice
    [ TRgba <$ string "rgba",
      TRgb <$ string "rgb",
      THsla <$ string "hsla",
      THsl <$ string "hsl",
      CInterpolate <$ string "interpolate"
    ]

-- | color function parser
pFColor :: ColorToken -> Parser Color
pFColor TRgb = pRgb
  where
    pRgb = betweenBrackets $ do
      r <- fromIntegral <$> pInt
      _ <- char ',' >> space
      g <- fromIntegral <$> pInt
      _ <- char ',' >> space
      b <- fromIntegral <$> pInt
      return $ rgbToColor r g b 1
pFColor TRgba = pRgba
  where
    pRgba = betweenBrackets $ do
      r <- fromIntegral <$> pInt
      _ <- char ',' >> space
      g <- fromIntegral <$> pInt
      _ <- char ',' >> space
      b <- fromIntegral <$> pInt
      _ <- char ',' >> space
      rgbToColor r g b <$> lexeme L.decimal
pFColor THsla = pHsla
  where
    pHsla = betweenBrackets $ do
      h <- pDouble
      _ <- char ',' >> space
      s <- pPercentage
      _ <- char ',' >> space
      l <- pPercentage
      _ <- char ',' >> space
      hslToColor h s l <$> pDouble
pFColor THsl = pHsl
  where
    pHsl = betweenBrackets $ do
      h <- lexeme L.decimal
      _ <- char ',' >> space
      s <- pPercentage
      _ <- char ',' >> space
      l <- pPercentage
      return $ hslToColor h s l 1

pHexColor :: Parser Color
pHexColor = betweenDoubleQuotes $ do
  _ <- char '#'
  hexDigits <- try (some hexDigitChar)
  let len = length hexDigits
  let validLength = len == 6 || len == 3
  if validLength
    then return $ colorFromHexDigits hexDigits
    else fail "Invalid hex color code length"

--------------------------------------------------------------------------------
-- Num symbol
--------------------------------------------------------------------------------

numSymbol :: Parser NumToken
numSymbol =
  choice
    [ Plus <$ char '+',
      Minus <$ char '-',
      Div <$ char '/',
      Multi <$ char '*',
      NInterpolate <$ string "interpolate",
      Zoom <$ string "zoom",
      IndexOf <$ string "index-of",
      Length <$ string "length"
    ]
    <|> NPoly
    <$> polySymbol

--------------------------------------------------------------------------------
-- String symbol
--------------------------------------------------------------------------------

stringSymbol :: Parser StringToken
stringSymbol =
  choice
    [ GeometryType <$ string "geometry-type",
      Upcase <$ string "upcase",
      Downcase <$ string "downcase",
      Concat <$ string "concat",
      TextAt <$ string "at"
    ]
    <|> SPoly
    <$> polySymbol

--------------------------------------------------------------------------------
-- Bool symbol
--------------------------------------------------------------------------------

boolSymbol :: Parser BoolToken
boolSymbol = choice [Negated Equality <$ string "!=", Equality <$ string "=="]

--------------------------------------------------------------------------------
-- Polymorphic symbol
--------------------------------------------------------------------------------

polySymbol :: Parser PolyToken
polySymbol = choice [Get <$ string "get", At <$ string "at"]
