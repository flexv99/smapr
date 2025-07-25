{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Style.Lang.Lex (
  Parser,
  betweenBrackets,
  betweenSquareBrackets,
  betweenDoubleQuotes,
  numSymbol,
  stringSymbol,
  boolSymbol,
  colorSymbol,
  arraySymbol,
  polySymbol,
  pAtom,
  pString,
  pNum,
  pBool,
  pColor,
  pArray,
  pFColor,
  escapednl,
)
where

import Data.Scientific (toRealFloat)
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
snakeCaseChar = alphaNumChar <|> char '_' <|> char '-' <|> char '’' <|> char ':'

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

escapednl :: Parser T.Text
escapednl = (char '\"' *> string "\\n" <* (char '\"'))

pString :: Parser SString
pString =
  nullableP
    ( try escapednl <|> T.pack
        <$> betweenDoubleQuotes
          ( lexeme
              (many snakeCaseChar)
          )
    )
    <?> "string"

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
  try pHexColor <|> betweenDoubleQuotes (pFColor =<< colorSymbol) <?> "color"

pAtom :: Parser SData
pAtom =
  choice
    [ DNum <$> pNum
    , DBool <$> pBool
    , try $ DColor <$> pColor
    , DString <$> pString
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

pArray :: Parser [SData]
pArray = do
  try oneElemArr
    <|> betweenSquareBrackets
      ( do
          firstElem <- pAtom
          _ <- char ',' >> space
          restElems <- parserForType firstElem `sepBy` (char ',' >> space)
          return (firstElem : restElems)
      )
    <?> "array"
  where
    oneElemArr = betweenSquareBrackets $ do
      val <- pAtom
      return [val]

parserForType :: SData -> Parser SData
parserForType t = case t of
  DNum _ -> DNum <$> pNum
  DBool _ -> DBool <$> pBool
  DString _ -> DString <$> pString
  DArray _ -> DArray <$> pArray
  _ -> pAtom

--------------------------------------------------------------------------------

colorSymbol :: Parser ColorToken
colorSymbol =
  choice
    [ TRgba <$ string "rgba"
    , TRgb <$ string "rgb"
    , THsla <$ string "hsla"
    , THsl <$ string "hsl"
    , CInterpolate <$ string "interpolate"
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
      rgbToColor r g b . toRealFloat <$> lexeme L.scientific
pFColor THsla = pHsla
  where
    pHsla = betweenBrackets $ do
      h <- toRealFloat <$> lexeme L.scientific
      _ <- char ',' >> space
      s <- pPercentage
      _ <- char ',' >> space
      l <- pPercentage
      _ <- char ',' >> space
      hslToColor h s l . toRealFloat <$> lexeme L.scientific
pFColor THsl = pHsl
  where
    pHsl = betweenBrackets $ do
      h <- toRealFloat <$> lexeme L.scientific
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
    [ Number <$ string "number"
    , Plus <$ char '+'
    , Minus <$ char '-'
    , Div <$ char '/'
    , Multi <$ char '*'
    , NInterpolate <$ string "interpolate"
    , Zoom <$ string "zoom"
    , IndexOf <$ string "index-of"
    , Length <$ string "length"
    , ToNumber <$ string "to-number"
    ]
    <|> NPoly
    <$> polyReciever

--------------------------------------------------------------------------------
-- String symbol
--------------------------------------------------------------------------------

stringSymbol :: Parser StringToken
stringSymbol =
  choice
    [ GeometryType <$ string "geometry-type"
    , Upcase <$ string "upcase"
    , Downcase <$ string "downcase"
    , Concat <$ string "concat"
    , TypeOf <$ string "typeof"
    , ToString <$ string "to-string"
    ]
    <|> SPoly
    <$> polyReciever

--------------------------------------------------------------------------------
-- Bool symbol
--------------------------------------------------------------------------------

boolSymbol :: Parser BoolToken
boolSymbol =
  choice
    [ Negated Equality <$ string "!="
    , try $ Neg <$ string "!"
    , Boolean <$ string "boolean"
    , Equality <$ string "=="
    , LessEq <$ string "<="
    , Less <$ char '<'
    , GreaterEq <$ string ">="
    , Greater <$ char '>'
    , In <$ string "in"
    , Has <$ string "has"
    , All <$ string "all"
    , Any <$ string "any"
    ]
    <|> BPoly
    <$> polyReciever

--------------------------------------------------------------------------------
-- Array symbol
--------------------------------------------------------------------------------

arraySymbol :: Parser ArrayToken
arraySymbol = choice [Array <$ string "array"] <|> APoly <$> polyReciever

--------------------------------------------------------------------------------
-- Polymorphic symbol
--------------------------------------------------------------------------------

polyReciever :: Parser PolyToken
polyReciever =
  choice
    [ Get <$ string "get"
    , At <$ string "at"
    , Match <$ string "match"
    , Case <$ string "case"
    , Step <$ string "step"
    , Coalesce <$ string "coalesce"
    ]

polySymbol :: Parser PolyToken
polySymbol =
  polyReciever
    <|> PArray
    <$> arraySymbol
      <|> PNum
    <$> numSymbol
      <|> PString
    <$> stringSymbol
      <|> PBool
    <$> boolSymbol
      <|> PColor
    <$> colorSymbol
