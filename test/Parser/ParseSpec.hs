{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}

module Parser.ParseSpec (spec) where

import qualified Data.Text.Lazy as T
import Test.Hspec
import Text.Megaparsec
import Style.Parser
import Style.IsoExpressions
import Style.FeatureExpressions
import Style.ExpressionsWrapper
import Style.ExpressionsEval
import Proto.Util
import Style.Poc

spec :: Spec
spec = do
  describe "Style.Parser.SType.SBool" $ do
    it "parse bool" $ do
      parseMaybe pAtom "false" `shouldBe` Just (SBool False)
  describe "Style.Types.Parser.SNumber" $ do
    it "parse number" $ do
      parseMaybe pAtom "1" `shouldBe` Just (SNum $ SInt 1)
  describe "Style.Parser.SType.SNumber" $ do
    it "parse signed number" $ do
      parseMaybe pAtom "-1" `shouldBe` Just (SNum $ SInt (-1))
  describe "Style.Parser.SType.SNumber" $ do
    it "parse float" $ do
      parseMaybe pAtom "1.001" `shouldBe` Just (SNum $ SDouble 1.001)
  describe "Style.Parser.SType.SString" $ do
    it "parse string" $ do
      parseMaybe pAtom "\"abc\"" `shouldBe` Just (SString "abc")
  describe "Style.Parser.SType.SString" $ do
    it "parse snake cased string" $ do
      parseMaybe pAtom "\"ab_c\"" `shouldBe` Just (SString "ab_c")
  describe "Style.Parser.SType.SColor" $ do
    it "parse hsl color" $ do
      fmap showSColor (parseMaybe pAtom "\"hsl(205,56%,73%)\"") `shouldBe` Just "#94c1e1"
  describe "Style.Parser.SType.SColor" $ do
    it "parse rgba color" $ do
      fmap showSColor (parseMaybe pAtom "\"rgba(179,222,155,0.6)\"") `shouldBe` Just "#b3de9b"
  describe "Style.Parser.SType.SColor" $ do
    it "parse rgb color" $ do
      fmap showSColor (parseMaybe pAtom "\"rgb(183,220,163)\"") `shouldBe` Just "#b7dca3"
  describe "Style.Parser.SType.SArray" $ do
    it "parse array" $ do
      parseMaybe pAtom "[-1, 0, 4]" `shouldBe` Just (SArray $ map SNum [SInt (-1), SInt 0, SInt 4])
  describe "Style.Parser.SType.SNull" $ do
    it "parse null" $ do
      parseMaybe pAtom "null" `shouldBe` Just SNull
  describe "Style.Expressions.sumP" $ do
    it "can parse and evaluate sum expression" $ do
      t <- testLayerAndFeature
      (\(l, f) -> eval . wrap <$> parseMaybe numRetExprP "[\"+\", 1, 2, 3, [\"-\", 1, 2]]" <*> f <*> l) t `shouldBe` Just (SNum $ SInt 5)
  describe "Style.Expressions.prodP" $ do
    it "can parse and evaluate multiplication expression" $ do
      t <- testLayerAndFeature
      (\(l, f) -> eval . wrap <$> parseMaybe numRetExprP "[\"*\", 1, [\"-\", 1, 2], 5, 6]" <*> f <*> l) t `shouldBe` Just (SNum $ SInt (-30))
  describe "Style.Expressions.subP" $ do
    it "can parse and evaluate subtraction expression" $ do
      t <- testLayerAndFeature
      (\(l, f) -> eval . wrap <$> parseMaybe numRetExprP "[\"-\", 1, [\"+\", 1, 2]]" <*> f <*> l) t `shouldBe` Just (SNum $ SInt (-2))
  describe "Style.Expressions.divP" $ do
    it "can parse and evaluate division expression" $ do
      t <- testLayerAndFeature
      (\(l, f) -> eval . wrap <$> parseMaybe numRetExprP "[\"/\", 1, [\"+\", 2, 2]]" <*> f <*> l) t `shouldBe` Just (SNum $ SDouble 0.25)
  describe "Style.Expressions.eqP" $ do
    it "can parse and evaluate eqality expression" $ do
      t <- testLayerAndFeature
      (\(l, f) -> eval . wrap <$> parseMaybe eqP "[\"==\",1, 1]" <*> f <*> l) t `shouldBe` Just (SBool True)
  describe "Style.Expressions.atP" $ do
    it "can parse and return element at index of a list" $ do
      t <- testLayerAndFeature
      (\(l, f) -> eval . wrap <$> (parseMaybe atP "[\"at\", [\"a\", \"bc\", \"tre\"], 1]" :: Maybe (ArgType ('SString s))) <*> f <*> l) t  `shouldBe` Just (SString "bc")
  describe "Style.FilterExpressions.fgeometryP" $ do
    it "parse geometry type function that retrieves a feature's geometry" $ do
      t <- testLayerAndFeature
      let expr = parseMaybe fgeometryP "[\"geometry-type\"]"
      (\ (a, b) -> eval . wrap <$> expr <*> b <*> a) t `shouldBe` Just (SString "LINESTRING")
  describe "Style.FilterExpressions.evalFilterExpr" $ do
    it "can evaluate a in Expression" $ do
      t <- testLayerAndFeature
      let expr = parseMaybe fInP "[\"!in\", \"brunnel\", \"tunnel\", \"bridge\"]"
      (\ (a, b) -> eval . wrap <$> expr <*> b <*> a) t `shouldBe` Just (SBool True)
  describe "Style.FilterExpressions.evalFilterGetter" $ do
    it "can evaluate a in getter Expression on feature properties" $ do
      t <- testLayerAndFeature
      let expr = parseMaybe fgetP "[\"get\",\"intermittent\"]"
      (\ (a, b) -> eval . wrap <$> expr <*> b <*> a) t `shouldBe` Just (SNum (SInt 0))
  describe "Style.ExpressionsEval.eval" $ do
    it "combination of feature and iso expressions" $ do
      t <- testLayerAndFeature
      let expr = wrap <$> parseMaybe eqP "[\"==\", [\"geometry-type\"], \"LINESTRING\"]"
      (\ (a, b) -> eval <$> expr <*> b <*> a) t `shouldBe` Just (SBool True)
  describe "Style.ExpressionsEval.eval" $ do
    it "combination of feature and iso expressions '" $ do
      t <- testLayerAndFeature
      let expr = wrap <$> parseMaybe eqP "[\"!=\",[\"get\",\"intermittent\"], 1]"
      (\ (a, b) -> eval <$> expr <*> b <*> a) t `shouldBe` Just (SBool True)
  describe "Style.ExpressionsEval.eval" $ do
    it "combination of feature and iso expressions ''" $ do
      t <- testLayerAndFeature
      let expr = wrap <$> parseMaybe eqP "[\"==\",[\"get\",\"intermittent\"],0]"
      (\ (a, b) -> eval <$> expr <*> b <*> a) t `shouldBe` Just (SBool True)
  describe "Style.ExpressionsEval.eval" $ do
    it "match expression" $ do
      t <- testLayerAndFeature
      let expr = wrap <$> (parseMaybe matchP "[\"match\", [\"get\", \"brunnel\"], [\"bridge\", \"tunnel\"], false, true]" :: Maybe (ArgType ('SBool b)))
      (\ (a, b) -> eval <$> expr <*> b <*> a) t `shouldBe` Just (SBool True)
  describe "Style.ExpressionsEval.eval" $ do
    it "interpolate expression linear" $ do
      t <- testLayerAndFeature
      let expr = wrap <$> parseMaybe interpolateP "[\"interpolate\",[\"linear\"],5,0, 100, 10, 200]"
      (\ (a, b) -> eval <$> expr <*> b <*> a) t `shouldBe` Just (SNum (SDouble 150))
  describe "Style.ExpressionsEval.eval" $ do
    it "interpolate expression exponential" $ do
      t <- testLayerAndFeature
      let expr = wrap <$> parseMaybe interpolateP "[\"interpolate\",[\"exponential\", 2],1,1,2,3,6]"
      (\ (a, b) -> eval <$> expr <*> b <*> a) t `shouldBe` Just (SNum (SDouble 2))
  describe "Style.ExpressionsEval.eval" $ do
    it "interpolate expression exponential '" $ do
      t <- testLayerAndFeature
      let expr = wrap <$> parseMaybe interpolateP "[\"interpolate\",[\"exponential\", 2],2,1,2,3,6]"
      (\ (a, b) -> eval <$> expr <*> b <*> a) t `shouldBe` Just (SNum (SDouble 3.333333333333333))
