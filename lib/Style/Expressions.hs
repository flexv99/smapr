{-# LANGUAGE OverloadedStrings, DeriveGeneric, FlexibleInstances, DataKinds, AllowAmbiguousTypes #-}

module Style.Expressions where

import GHC.Generics (Generic)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Aeson as A
import qualified Data.Aeson.Text as A
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Text.Lazy as T
import Data.List (isInfixOf)
import Style.Parser

data Expression
  = SLitType SType
  | SAtType SAt
  | SInType SIn
  | SIndexOfType SIndexOf
  | SGetType SGet
  | SEqType SEq
  deriving (Show, Eq)

data BoolRetExpr
  = SLitBool SType
  | SInBool SIn
  | SEqBool SEq
  | SAllBool SAll
  | FSInBool FSIn
  | FSEqBool FSEq
  deriving (Show, Eq)

class Expr a where
  parseExpr :: Parser a
  evaluate  :: a -> SType


exprP :: Parser Expression
exprP = label "Expression" $ choice $ map try
      [ SLitType     <$> pAtom
      , SEqType      <$> eqP
      , SInType      <$> inP
      , SIndexOfType <$> indexOfP
      , SGetType     <$> getP
      , SAtType      <$> atP
      ]

boolRetExprP :: Parser BoolRetExpr
boolRetExprP = label "bool | bool returning expr" $ choice $ map try
  [ SLitBool  <$> pBool
  , SInBool   <$> inP
  , SEqBool   <$> eqP
  , SAllBool  <$> allP
  , FSInBool  <$> fInP
  , FSEqBool  <$> feqP
  ]

-- | to be used every time a expr needs more args of the same type
typeEqAssert :: SType -> Parser SType
typeEqAssert (SInt _)      = pInteger
typeEqAssert (SDouble _)   = pDouble
typeEqAssert (SString _)   = pString
typeEqAssert (SBool _)     = pBool
typeEqAssert (SArray _)    = pArray
typeEqAssert (STypeType _) = pType
typeEqAssert _             = error "does not match any supported type"

--- LOOKUP:
{-
at
Retrieves an item from an array.
["at", value, number]: value
-}
data SAt = SAt { array :: SType
               , index :: SType
               } deriving (Show, Generic, Eq)

atId :: T.Text
atId = "at"

atP :: Parser SAt
atP = label (show atId) $
  betweenSquareBrackets $ do
    key <- pKeyword atId
    lexeme (char ',')
    value <- literal
    lexeme (char ',')
    idx <- pInteger
    return SAt {array = value, index = idx}

instance Expr SAt where
  parseExpr = atP

  evaluate (SAt (SArray a) (SInt i)) = a !! i
  evaluate (SAt _ (SInt i))          = error "at arg1 must be an array"
  evaluate (SAt (SArray a) _)        = error "at arg2 must be an int"


{-
in & !in
Determines whether an item exists in an array or a substring exists in a string.
["in", value, value]: boolean
-}
data SIn = SIn { object :: SType
               , item :: SType
               } deriving (Show, Generic, Eq)

inId :: T.Text
inId = "in"

inP :: Parser SIn
inP = label (show inId) $
  betweenSquareBrackets $ do
    key <- pKeyword inId
    lexeme (char ',')
    obj <- pAtom
    lexeme (char ',')
    itm <- pAtom
    return SIn { object = obj, item = itm }


{-
index-of
Returns the first position at which an item can be found in an array or a substring can be found in a string,
or -1 if the input cannot be found. Accepts an optional index from where to begin the search.
["index-of", value, value, number?]: number
-}
data SIndexOf = SIndexOf { lookupItem :: SType
                         , items :: SType
                         , startIndex :: Maybe SType
                         } deriving (Show, Generic, Eq)

indexOfId :: T.Text
indexOfId = "index-of"

indexOfP :: Parser SIndexOf
indexOfP = label (show indexOfId) $
  betweenSquareBrackets $ do
    key <- pKeyword indexOfId
    lexeme (char ',')
    lookup <- pAtom
    lexeme (char ',')
    onItems <- pArray
    start <- optional $ do
      lexeme (char ',')
      pInteger
    return SIndexOf { lookupItem = lookup, items = onItems, startIndex = start }

{-
get
Retrieves a property value from the current feature's properties,
or from another object if a second argument is provided.
Returns null if the requested property is missing.
["get", string]: value
-}
newtype SGet = SGet { runGet :: SType } deriving (Show, Generic, Eq)

getId :: T.Text
getId = "get"

-- Parse a getter expression
getP :: Parser SGet
getP = label (show getId) $
  betweenSquareBrackets $ do
    key <- pKeyword getId
    lexeme (char ',')
    SGet <$> pAtom

{-
== & !=
Returns true if the input values are equal, false otherwise.
The comparison is strictly typed: values of different runtime types are always considered unequal.
Cases where the types are known to be different at parse time are considered invalid and will produce a parse error.
Accepts an optional collator argument to control locale-dependent string comparisons.
-}
data SEq = SEq { iOne :: SType
               , iTwo :: SType
               } deriving (Show, Generic, Eq)

eqId :: T.Text
eqId = "=="

eqP :: Parser SEq
eqP = label (show eqId) $
  betweenSquareBrackets $ do
    key <- pKeyword eqId
    lexeme (char ',')
    item1 <- pAtom
    lexeme (char ',')
    item2 <- typeEqAssert item1
    return SEq { iOne = item1, iTwo = item2 }

{-
all
Returns true if all the inputs are true, false otherwise. The inputs are evaluated in order,
and evaluation is short-circuiting: once an input expression evaluates to false,
the result is false and no further input expressions are evaluated.
-}
newtype SAll = SAll { args :: [BoolRetExpr] } deriving (Show, Generic, Eq)

allId :: T.Text
allId = "all"

allP :: Parser SAll
allP = label (show allId) $
  betweenSquareBrackets $ do
    key <- pKeyword allId
    lexeme (char ',')
    arguments <- boolRetExprP `sepBy` (char ',' >> space)
    return SAll { args = arguments }


instance A.FromJSON Expression where
    parseJSON = A.withArray "Expression" $ \v ->
                case parse exprP "" (A.encodeToLazyText v) of
                  Left err  -> fail $ errorBundlePretty err
                  Right res -> return res

-- parseTest allP "[\"all\",[\"==\",\"type\",\"LineString\"], true,[\"==\",\"type\",\"LineString\"]]"
-- A.eitherDecode "[\"==\",\"$type\",\"LineString\"]" :: Either String Expression
-- A.eitherDecode "[\"at\", [\"literal\", [\"a\", \"b\", \"c\"]], 1]" :: Either String Expression
-- A.eitherDecode "[\"all\",[\"==\",\"$type\",\"LineString\"],[\"!in\",\"brunnel\",\"tunnel\",\"bridge\"]]" :: Either String Expression


data FSIn = FSIn { obj :: [SType]
                 } deriving (Show, Generic, Eq)

fInId :: T.Text
fInId = "!in"

fInP :: Parser FSIn
fInP = label (show fInId) $
  betweenSquareBrackets $ do
    key <- pKeyword fInId
    lexeme (char ',')
    obj <- pAtom `sepBy` lexeme (char ',')
    return FSIn { obj = obj }


data FSEq = FSEq { fiOne :: SType
                 , fiTwo :: SType
                 } deriving (Show, Generic, Eq)

feqId :: T.Text
feqId = "=="

feqP :: Parser FSEq
feqP = label (show feqId) $
  betweenSquareBrackets $ do
    key <- pKeyword feqId
    lexeme (char ',')
    item1 <- pType
    lexeme (char ',')
    item2 <- pAtom
    return FSEq { fiOne = item1, fiTwo = item2 }
