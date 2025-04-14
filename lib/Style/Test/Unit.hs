{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Style.Test.Unit where

import Control.Lens
import Control.Monad.Except (MonadError, liftEither, runExceptT, throwError)
import Control.Monad.Reader
import qualified Data.Aeson as A
import qualified Data.Aeson.Text as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Either
import qualified Data.Map as MP
import Data.Maybe
import Data.Scientific
import qualified Data.Sequence as S
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Vector as V
import Proto.Vector_tile.Tile.Feature
import Proto.Vector_tile.Tile.Layer hiding (ext'field)
import Proto.Vector_tile.Tile.Value
import Style.ExpressionsContext
import Style.Lang.Ast
import Style.Lang.Eval
import Style.Lang.Lex
import Style.Lang.Parser
import Style.Lang.Types
import Text.Megaparsec
import qualified Text.ProtocolBuffers.Header as P'
import Util

data ResultType = RNumber | RBoolean | RArray | RString | RColor deriving (Show)

instance A.FromJSON ResultType where
  parseJSON = A.withText "result-type" $ \case
    "number" -> return RNumber
    "boolean" -> return RBoolean
    "array" -> return RArray
    "string" -> return RString
    "color" -> return RColor
    _ -> error "not supported result type"

data ECompiled = ECompiled
  { _rType :: ResultType
  , _result :: String
  }
  deriving (Show)

makeLenses ''ECompiled

instance A.FromJSON ECompiled where
  parseJSON = A.withObject "ECompiled" $ \t -> ECompiled <$> t A..: "type" <*> t A..: "result"

data EExpected where
  EExpected :: {_outputs :: [Maybe SData], _compiled :: ECompiled} -> EExpected

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

pLiterals :: A.Value -> A.Parser [Maybe SData]
pLiterals = A.withArray "list of literals" (return . V.toList . V.map (parseMaybe pAtom . A.encodeToLazyText))

type Properties = (MP.Map String (MP.Map String SData))

data ExpressionTestEntity = ExpressionTestEntity
  { _expression :: Maybe (SExpr SData)
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
      exprP :: Maybe A.Value -> A.Parser (Maybe (SExpr SData))
      exprP Nothing = pure Nothing
      exprP (Just v) = case parse polyExprP "" (A.encodeToLazyText v) of
        Left err -> fail $ errorBundlePretty err
        Right res -> pure $ Just res

sDataToValue :: SData -> Value
sDataToValue (DString s) = (P'.defaultValue :: Value){string_value = (fromEither . P'.toUtf8 . T.encodeUtf8) =<< s}
  where
    fromEither (Right a) = Just a
    fromEither _ = Nothing
sDataToValue (DNum d) = (P'.defaultValue :: Value){double_value = toRealFloat <$> d}
sDataToValue (DBool b) = (P'.defaultValue :: Value){bool_value = b}
sDataToValue (DArray d) = P'.defaultValue :: Value -- cannot store arrays to value
sDataToValue _ = error "unsupported type"

testCTXs :: Properties -> ExpressionContext
testCTXs p = maybe defaultCtx (\x -> ExpressionContext{_ctxZoom = 14, _layer = x, _feature = dFeature}) createLayer
  where
    props = MP.lookup "properties" p
    nrProps = maybe 0 (length . MP.keys) props
    k' = S.fromList . rights . map (P'.toUtf8 . BC.pack) . MP.keys <$> props
    v' = S.fromList . map sDataToValue . MP.elems <$> props
    t' = S.fromList $ map fromInteger $ take (nrProps * 2) $ mconcat $ zipWith (\a b -> a : [b]) [0 ..] [0 ..]
    dFeature = (P'.defaultValue :: Feature){tags = t'}
    createLayer = (\y -> fmap (\x -> (P'.defaultValue :: Layer){keys = x, values = y, features = S.singleton dFeature}) k') =<< v'
    defaultCtx = ExpressionContext{_ctxZoom = 14, _layer = P'.defaultValue, _feature = P'.defaultValue}

runTestWithResult :: (MonadError String m, MonadIO m) => m [Maybe SData]
runTestWithResult = do
  t <- liftIO readTest >>= liftEither -- Run IO action and lift Either into MonadError
  return $ fromMaybe [] (testWithContexts t)
  where
    testWithContexts t =
      fmap
        (\r -> map (runReader r <$>) (tContexts t))
        (eval <$> view expression t)
    tContexts t = map (testCTXs <$>) ((!! 1) <$> view inputs t)

runTest :: (MonadError String m, MonadIO m) => m [Maybe Bool]
runTest = do
  t <- liftIO readTest >>= liftEither -- Run IO action and lift Either into MonadError
  let results = fromMaybe [] (testWithContexts t)
  return $ zipWith (\a b -> (==) <$> a <*> b) results (expectedRes t)
  where
    testWithContexts t =
      fmap
        (\r -> map (runReader r <$>) (tContexts t))
        (eval <$> view expression t)
    tContexts t = map (testCTXs <$>) ((!! 1) <$> view inputs t)
    expectedRes t = t ^. (expected . outputs)

readTest :: IO (Either String ExpressionTestEntity)
readTest = do
  conf <- smaprConfig
  let testPath' = jsonTestPath conf ++ "any/basic/test.json"
  tf <- B.readFile testPath'
  return $ A.eitherDecode tf

-- >>> t <- run
-- >>> map (\x -> testCTXs <$> x) (fmap (\x -> (x !! 1)) $ view inputs t)
-- fmap (\r -> runReader r <$> (head contexts)) (eval <$> (view expression (unwap t)))

-- >> runExceptT runTest
