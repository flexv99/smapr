{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}

module Style.IsoExpressions where

import Data.Kind (Type)
import qualified Data.Text.Lazy as T
import qualified Text.Megaparsec.Char.Lexer as L
import Proto.Vector_tile.Tile.Feature (Feature(..))
import Proto.Vector_tile.Tile.Layer (Layer(..))
import Text.Megaparsec
import Text.Megaparsec.Char
import Style.ExpressionsWrapper
import Style.Parser

stringExprP :: Parser (IsoExpr (SString s))
stringExprP = StringE <$> pString <* hidden space

intExprP :: Parser (IsoExpr (SNum (SInt i)))
intExprP = IntE <$> pInteger <* hidden space

doubleExprP :: Parser (IsoExpr (SNum (SDouble d)))
doubleExprP = DoubleE <$> pDouble <* hidden space

boolExprP :: Parser (IsoExpr (SBool b))
boolExprP = BoolE <$> pBool <* hidden space

arrayExprP :: Parser (IsoExpr (SArray a))
arrayExprP = ArrayE <$> arrayLitP <* hidden space

exprChoicheP :: Parser WrappedExpr
exprChoicheP = choice [ wrap <$> intExprP
                      , wrap <$> doubleExprP
                      , wrap <$> boolExprP
                      , wrap <$> stringExprP
                      , wrap <$> arrayExprP]

stypeSum :: [SType] -> SType
stypeSum = foldr stypeAdd (SNum $ SInt 0)
  where
    stypeAdd :: SType -> SType -> SType
    stypeAdd (SNum (SInt i))    (SNum (SInt j))     = SNum $ SInt $ i + j
    stypeAdd (SNum (SInt i))    (SNum (SDouble j))  = SNum $ SDouble $ fromIntegral i + j
    stypeAdd (SNum (SDouble i)) (SNum (SInt j))     = SNum $ SDouble $ i + fromIntegral j
    stypeAdd (SNum (SDouble i)) (SNum (SDouble j))  = SNum $ SDouble $ i + j
    stypeAdd _ _                      = error "must be numeric type"

stypeProd :: [SType] -> SType
stypeProd = foldr stypeProd (SNum $ SInt 1)
  where
    stypeProd :: SType -> SType -> SType
    stypeProd (SNum (SInt i))    (SNum (SInt j))  = SNum $ SInt $ i * j
    stypeProd (SNum (SInt i))    (SNum (SDouble j))  = SNum $ SDouble $ fromIntegral i * j
    stypeProd (SNum (SDouble i)) (SNum (SInt j))     = SNum $ SDouble $ i * fromIntegral j
    stypeProd (SNum (SDouble i)) (SNum (SDouble j))  = SNum $ SDouble $ i * j
    stypeProd _ _                      = error "must be numeric type"

stypeSub :: SType -> SType -> SType
stypeSub (SNum (SInt i))    (SNum (SInt j))    = SNum $ SInt $ i - j
stypeSub (SNum (SInt i))    (SNum (SDouble j)) = SNum $ SDouble $ fromIntegral i - j
stypeSub (SNum (SDouble i)) (SNum (SInt j))    = SNum $ SDouble $ i - fromIntegral j
stypeSub (SNum (SDouble i)) (SNum (SDouble j)) = SNum $ SDouble $ i - j
stypeSub _ _                     = error "must be numeric type"

stypeDiv :: SType -> SType -> SType
stypeDiv (SNum (SInt i))    (SNum (SInt j))    = SNum $ SDouble $ fromIntegral i / fromIntegral j
stypeDiv (SNum (SInt i))    (SNum (SDouble j)) = SNum $ SDouble $ fromIntegral i / j
stypeDiv (SNum (SDouble i)) (SNum (SInt j))    = SNum $ SDouble $ i / fromIntegral j
stypeDiv (SNum (SDouble i)) (SNum (SDouble j)) = SNum $ SDouble $ i / j
stypeDiv _ _                     = error "must be numeric type"

stypeEq :: SType -> SType -> SType
stypeEq (SNum i)    (SNum j)    = SBool $ i == j
stypeEq (SString i) (SString j) = SBool $ i == j
stypeEq (SBool i)   (SBool j)   = SBool $ i == j
stypeEq (SArray i)  (SArray j)  = SBool $ i == j
stypeEq _ _                     = error "eq on not supported types"

stypeIn :: SType -> SType -> SType
stypeIn (SArray a) (SNum (SInt i)) = a !! i
stypeIn _          (SNum (SInt i)) = error "param 1 must be an array"
stypeIn (SArray a) _               = error "param 2 must be an int"

-- >>> fmap evalExpr $ parseMaybe numRetExprP "[\"+\", 1, [\"/\", 1, 2]]"
-- 1.5
numRetExprP :: Parser (IsoExpr a)
numRetExprP = choice $ map try [ AddE . SArray <$> exprBaseP "+"  (singleArgP  `sepBy` (char ',' >> space))
                               , exprBaseP "-" $ SubE <$> argWithComma <*> singleArgP
                               , ProdE . SArray <$> exprBaseP "*" (singleArgP `sepBy` (char ',' >> space))
                               , exprBaseP "/" $ DivE <$> argWithComma <*> singleArgP
                               ]
              where
                singleArgP = (wrap <$> numberLitP) <|> (wrap <$> numRetExprP)
                argWithComma = do
                  val <- singleArgP
                  _ <- char ',' >> space
                  return val

-- >>> fmap evalExpr $ parseMaybe eqP "[\"==\", [1, 2, 3], [123]]"
-- false
-- >> evalExpr <$> parseMaybe eqP "[\"==\", [\"+\", 123, 4], 127]"
-- true
eqP :: Parser (IsoExpr ('SBool b))
eqP = exprBaseP "==" $ do
  val1 <- try exprChoicheP <|> (wrap <$> (numRetExprP :: (Parser (IsoExpr (SNum i)))))
  _ <- char ',' >> space
  EqE val1 <$> exprChoicheP


atP :: Parser (IsoExpr a)
atP = exprBaseP "at" $ do
  val1 <- arrayLitP
  _ <- char ',' >> space
  AtE val1 <$> intExprP
