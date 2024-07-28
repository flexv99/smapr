{-# LANGUAGE OverloadedStrings #-}

module Parser.ParseSpec (spec) where

import qualified Data.Text.Lazy as T
import qualified Data.Sequence as S
import Test.Hspec
import Text.Megaparsec
import Style.Parser
import Style.Expressions
import Style.FilterExpressions
import Proto.Util
import Proto.Vector_tile.Tile.Layer
import Proto.Vector_tile.Tile.Feature
import ApiClient

spec :: Spec
spec = do
  describe "Style.Parser.SType.SBool" $ do
    it "parse bool" $ do
      parseMaybe pAtom "false" `shouldBe` Just (SBool False)
  describe "Style.Types.Parser.SNumber" $ do
    it "parse number" $ do
      parseMaybe pAtom "1" `shouldBe` Just (SInt 1)
  describe "Style.Parser.SType.SNumber" $ do
    it "parse signed number" $ do
      parseMaybe pAtom "-1" `shouldBe` Just (SInt (-1))
  describe "Style.Parser.SType.SNumber" $ do
    it "parse float" $ do
      parseMaybe pAtom "1.001" `shouldBe` Just (SDouble 1.001)
  describe "Style.Parser.SType.SString" $ do
    it "parse string" $ do
      parseMaybe pAtom "\"abc\"" `shouldBe` Just (SString "abc")
  describe "Style.Parser.SType.SString" $ do
    it "parse snake cased string" $ do
      parseMaybe pAtom "\"ab_c\"" `shouldBe` Just (SString "ab_c")
  describe "Style.Parser.SType.SColor" $ do
    it "parse hsl color" $ do
      fmap showSColor (parseMaybe pHslColor "hsl(205,56%,73%)") `shouldBe` Just "#94c1e1"
  describe "Style.Parser.SType.SType" $ do
    it "parse array" $ do
      parseMaybe arrayLitP "[-1, 0, 4]" `shouldBe` Just (SArray [SInt (-1), SInt 0, SInt 4])
  describe "Style.Expressions.sumP" $ do
    it "can parse and evaluate sum expression" $ do
      evalExpr <$> parseMaybe numRetExprP "[\"+\", 1, 2, 3, [\"-\", 1, 2]]" `shouldBe` Just (SInt 5)
  describe "Style.Expressions.prodP" $ do
    it "can parse and evaluate multiplication expression" $ do
      evalExpr <$> parseMaybe numRetExprP "[\"*\", 1, [\"-\", 1, 2], 5, 6]" `shouldBe` Just (SInt (-30))
  describe "Style.Expressions.subP" $ do
    it "can parse and evaluate subtraction expression" $ do
      evalExpr <$> parseMaybe numRetExprP "[\"-\", 1, [\"+\", 1, 2]]" `shouldBe` Just (SInt (-2))
  describe "Style.Expressions.divP" $ do
    it "can parse and evaluate division expression" $ do
      evalExpr <$> parseMaybe numRetExprP "[\"/\", 1, [\"+\", 2, 2]]" `shouldBe` Just (SDouble 0.25)
  describe "Style.Expressions.eqP" $ do
    it "can parse and evaluate eqality expression" $ do
      evalExpr <$> parseMaybe eqP "[\"==\",1, 1]" `shouldBe` Just (SBool True)
  describe "Style.Expressions.atP" $ do
    it "can parse and return element at index of a list" $ do
      evalExpr <$> parseMaybe atP "[\"at\", [\"a\", \"bc\", \"tre\"], 1]" `shouldBe` Just (SString "bc")
  describe "Style.FilterExpressions.evalFilterExpr" $ do
    it "can evaluate a in Expression" $ do
      t <- testLayerAndFeature
      let expr = parseMaybe fInP "[\"!in\", \"brunnel\", \"tunnel\", \"bridge\"]"
      (\ (a, b) -> evalFilterExpr <$> expr <*> b <*> a) t `shouldBe` Just True


waterLayerStyle :: T.Text
waterLayerStyle = "{\"id\":\"waterway\",\"type\":\"line\",\"source\":\"openmaptiles\",\"source-layer\":\"waterway\",\"filter\":[\"all\",[\"==\",\"$type\",\"LineString\"],[\"!in\",\"brunnel\",\"tunnel\",\"bridge\"],[\"!=\",\"intermittent\",1]],\"layout\":{\"visibility\":\"visible\"},\"paint\":{\"line-color\":\"hsl(205,56%,73%)\",\"line-opacity\":1,\"line-width\":{\"base\":1.4,\"stops\":[[8,1],[20,8]]}}}"

testLayerAndFeature :: IO (Maybe Layer, Maybe Feature)
testLayerAndFeature = do
  testLayer <- waterLayer
  let f = fmap (`S.index` 0) (features <$> testLayer)
  return (testLayer, f)
