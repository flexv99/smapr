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
import qualified Data.Map as MP
import qualified Data.Sequence as S
import qualified Data.Vector as V
import Proto.Vector_tile.Tile.Feature
import Proto.Vector_tile.Tile.Layer
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
  { _rType :: ResultType,
    _result :: String
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
  { _expression :: Maybe WrappedExpr,
    _inputs :: [[Maybe Properties]],
    _expected :: EExpected
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

testCTXs :: [Properties] -> [ExpressionContext]
testCTXs p = undefined
  where
    k' = map (S.fromList . MP.keys) p
    v' = map (S.fromList . MP.elems) p
    t' = take (length p * 2) $ mconcat $ zipWith (\a b -> a : [b]) [0 ..] [0 ..]
    dLayer = P'.defaultValue :: Layer
    dFeature = P'.defaultValue :: Feature
