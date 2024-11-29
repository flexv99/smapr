{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Style.Lang.Lex (Parser) where

import qualified Data.Text.Lazy as T
import Data.Void
import Style.Lang.Token
import Style.Lang.Types
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
pNum = nullableP (lexeme (L.signed sc L.scientific) <|> L.scientific) <?> "number"

--------------------------------------------------------------------------------

colorSymbol :: Parser ColorToken
colorSymbol =
  choice
    [ TRgb <$ string "rgb",
      TRgba <$ string "rgba",
      THsl <$ string "hsl",
      THsla <$ string "hsla"
    ]
