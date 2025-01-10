{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Style.Test.Entity where

import Control.Lens
import qualified Data.Aeson as A
import qualified Data.Aeson.Text as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Either
import qualified Data.Map as MP
import Data.Maybe
import qualified Data.Sequence as S
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Vector as V
import Proto.Vector_tile.Tile.Feature
import Proto.Vector_tile.Tile.Layer
import Proto.Vector_tile.Tile.Value
import Style.ExpressionsContext
import Style.ExpressionsWrapper
import Style.IsoExpressions
import Style.Parser
import Text.Megaparsec
import qualified Text.ProtocolBuffers.Header as P'

-- {
--     propertySpec: any;
--     expression: any[];
--     inputs:any[];
--     expected: {
--         compiled?: {
--             result?: any;
--             isFeatureConstant?: any;
--             isZoomConstant?: any;
--             type?: any;
--         };
--         outputs? : any;
--     };

data ResultType = RNumber | RBoolean | RArray | RString | RColor deriving (Show)

instance A.FromJSON ResultType where
  parseJSON = A.withText "result-type" $ \case
    "number" -> return RNumber
    "boolean" -> return RBoolean
    "array" -> return RArray
    "string" -> return RString
    "color" -> return RColor
    _ -> error "not supported result type"

-- boolean, number,

data ECompiled = ECompiled
  { _rType :: ResultType
  , _result :: String
  }
  deriving (Show)

makeLenses ''ECompiled

instance A.FromJSON ECompiled where
  parseJSON = A.withObject "ECompiled" $ \t -> ECompiled <$> t A..: "type" <*> t A..: "result"

data EExpected where
  EExpected :: {_outputs :: [Maybe SType], _compiled :: ECompiled} -> EExpected

deriving instance Show EExpected

makeLenses ''EExpected

instance A.FromJSON EExpected where
  parseJSON = A.withObject "Outputs" $ \t -> do
    comp <- t A..: "compiled"
    out <-
      ( t A..:? "outputs"
          >>= ( \case
                  Nothing -> pure Nothing
                  Just v -> sequenceA $ pure $ pLiterals v
              )
        )
        A..!= []
    return $ EExpected out comp

pLiterals :: A.Value -> A.Parser [Maybe SType]
pLiterals = A.withArray "list of literals" (return . V.toList . V.map (parseMaybe pAtom . A.encodeToLazyText))

type Properties = (MP.Map String (MP.Map String SType))

data ExpressionTestEntity = ExpressionTestEntity
  { _expression :: Maybe WrappedExpr
  , _inputs :: [[Maybe Properties]]
  , _expected :: EExpected
  }

deriving instance Show ExpressionTestEntity

makeLenses ''ExpressionTestEntity

instance A.FromJSON ExpressionTestEntity where
  parseJSON = A.withObject "ExpressionTest" $ \p -> do
    expr <- p A..: "expression" >>= exprP
    inputs' <- p A..: "inputs"
    expected' <- p A..: "expected"
    return $ ExpressionTestEntity expr inputs' expected'
    where
      exprP :: Maybe A.Value -> A.Parser (Maybe WrappedExpr)
      exprP Nothing = pure Nothing
      exprP (Just v) = case parse polyExprP "" (A.encodeToLazyText v) of
        Left err -> fail $ errorBundlePretty err
        Right res -> pure $ Just res

run :: IO (Either String ExpressionTestEntity)
run = do
  let testPath = "/home/flex99/dev/smapr/test/json_test/test_2.json"
  tf <- B.readFile testPath
  return $ A.eitherDecode tf

stypeToValue :: SType -> Value
stypeToValue (SString s) = (P'.defaultValue :: Value){string_value = fromEither $ P'.toUtf8 $ T.encodeUtf8 s}
  where
    fromEither (Right a) = Just a
    fromEither _ = Nothing
stypeToValue (SNum (SDouble d)) = (P'.defaultValue :: Value){double_value = Just d}
stypeToValue (SNum (SInt i)) = (P'.defaultValue :: Value){int_value = Just $ fromInteger $ toInteger i}
stypeToValue (SBool b) = (P'.defaultValue :: Value){bool_value = Just b}
stypeToValue _ = error "unsupported type"

testCTXs :: Properties -> Maybe ExpressionContext
testCTXs p = fmap (\x -> ExpressionContext{_ctxZoom = 14, _layer = x, _feature = dFeature}) createLayer
  where
    props = MP.lookup "properties" p
    k' = S.fromList . rights . map (P'.toUtf8 . BC.pack) . MP.keys <$> props
    v' = S.fromList . map stypeToValue . MP.elems <$> props
    t' = S.fromList $ map fromInteger $ take (length p * 2) $ mconcat $ zipWith (\a b -> a : [b]) [0 ..] [0 ..]
    dFeature = (P'.defaultValue :: Feature){tags = t'}
    createLayer = (\y -> fmap (\x -> (P'.defaultValue :: Layer){keys = x, values = y, features = S.singleton dFeature}) k') =<< v'

-- >>> t <- run
-- >>> map (\x -> testCTXs <$> x) (fmap (\x -> (x !! 1)) $ view inputs t)
