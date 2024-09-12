{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser.ParseSpec (spec) where

import Proto.Util
import Style.ExpressionsEval
import Style.ExpressionsWrapper
import Style.FeatureExpressions
import Style.IsoExpressions
import Style.Parser
import Test.Hspec
import Text.Megaparsec

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
  describe "Style.Parser.SType.SColor" $ do
    it "parse hex color" $ do
      fmap showSColor (parseMaybe pAtom "\"#00aaff\"") `shouldBe` Just "#00aaff"
  describe "Style.Parser.SType.SColor" $ do
    it "parse short hex color" $ do
      fmap showSColor (parseMaybe pAtom "\"#0af\"") `shouldBe` Just "#00aaff"
  describe "Style.Parser.SType.SArray" $ do
    it "parse array" $ do
      parseMaybe pAtom "[-1, 0, 4]" `shouldBe` Just (SArray $ map SNum [SInt (-1), SInt 0, SInt 4])
  describe "Style.Parser.SType.SNull" $ do
    it "parse null" $ do
      parseMaybe pAtom "null" `shouldBe` Just SNull
  describe "Style.Expressions.sumP" $ do
    it "can parse and evaluate sum expression" $ do
      ctx <- testLayerAndFeature
      (eval . wrap <$> parseMaybe numExprP "[\"+\", 1, 2, 3, [\"-\", 1, 2]]" <*> ctx) `shouldBe` Just (SNum $ SInt 5)
  describe "Style.Expressions.prodP" $ do
    it "can parse and evaluate multiplication expression" $ do
      ctx <- testLayerAndFeature
      (eval . wrap <$> parseMaybe numExprP "[\"*\", 1, [\"-\", 1, 2], 5, 6]" <*> ctx) `shouldBe` Just (SNum $ SInt (-30))
  describe "Style.Expressions.subP" $ do
    it "can parse and evaluate subtraction expression" $ do
      ctx <- testLayerAndFeature
      (eval . wrap <$> parseMaybe numExprP "[\"-\", 1, [\"+\", 1, 2]]" <*> ctx) `shouldBe` Just (SNum $ SInt (-2))
  describe "Style.Expressions.divP" $ do
    it "can parse and evaluate division expression" $ do
      ctx <- testLayerAndFeature
      (eval . wrap <$> parseMaybe numExprP "[\"/\", 1, [\"+\", 2, 2]]" <*> ctx) `shouldBe` Just (SNum $ SDouble 0.25)
  describe "Style.Expressions.eqP" $ do
    it "can parse and evaluate eqality expression" $ do
      ctx <- testLayerAndFeature
      (eval . wrap <$> parseMaybe eqP "[\"==\",1, 1]" <*> ctx) `shouldBe` Just (SBool True)
  describe "Style.Expressions.atP" $ do
    it "can parse and return element at index of a list" $ do
      ctx <- testLayerAndFeature
      (eval . wrap <$> (parseMaybe atP "[\"at\", [\"a\", \"bc\", \"tre\"], 1]" :: Maybe (ArgType ('SString s))) <*> ctx) `shouldBe` Just (SString "bc")
  describe "Style.Expressions.inP" $ do
    it "can parse and evaluate in expressions" $ do
      ctx <- testLayerAndFeature
      (eval . wrap <$> parseMaybe inP "[\"in\", 1, [9, 8, 7]]" <*> ctx) `shouldBe` Just (SBool False)
  describe "Style.Expressions.inP" $ do
    it "can parse and evaluate in expressions" $ do
      ctx <- testLayerAndFeature
      (eval . wrap <$> parseMaybe inP "[\"in\", \"abc\", \"abcz\"]" <*> ctx) `shouldBe` Just (SBool True)
  describe "Style.FilterExpressions.fgeometryP" $ do
    it "parse geometry type function that retrieves a feature's geometry" $ do
      ctx <- testLayerAndFeature
      let expr = parseMaybe fgeometryP "[\"geometry-type\"]"
      (eval . wrap <$> expr <*> ctx) `shouldBe` Just (SString "LINESTRING")
  describe "Style.FilterExpressions.evalFilterGetter" $ do
    it "can evaluate a in getter Expression on feature properties" $ do
      ctx <- testLayerAndFeature
      let expr = parseMaybe fgetP "[\"get\",\"intermittent\"]" :: Maybe (ArgType ('SNum i))
      (eval . wrap <$> expr <*> ctx) `shouldBe` Just (SNum (SInt 0))
  describe "Style.ExpressionsEval.eval" $ do
    it "combination of feature and iso expressions" $ do
      ctx <- testLayerAndFeature
      let expr = wrap <$> parseMaybe eqP "[\"==\", [\"geometry-type\"], \"LINESTRING\"]"
      (eval <$> expr <*> ctx) `shouldBe` Just (SBool True)
  describe "Style.ExpressionsEval.eval" $ do
    it "combination of feature and iso expressions '" $ do
      ctx <- testLayerAndFeature
      let expr = wrap <$> parseMaybe eqP "[\"!=\",[\"get\",\"intermittent\"], 1]"
      (eval <$> expr <*> ctx) `shouldBe` Just (SBool True)
  describe "Style.ExpressionsEval.eval" $ do
    it "combination of feature and iso expressions ''" $ do
      ctx <- testLayerAndFeature
      let expr = wrap <$> parseMaybe eqP "[\"==\",[\"get\",\"intermittent\"],0]"
      (eval <$> expr <*> ctx) `shouldBe` Just (SBool True)
  describe "Style.ExpressionsEval.eval" $ do
    it "match expression" $ do
      ctx <- testLayerAndFeature
      let expr = wrap <$> (parseMaybe matchP "[\"match\", [\"get\", \"brunnel\"], [\"bridge\", \"tunnel\"], false, true]" :: Maybe (ArgType ('SBool b)))
      (eval <$> expr <*> ctx) `shouldBe` Just (SBool True)
  describe "Style.ExpressionsEval.eval" $ do
    it "case expression" $ do
      ctx <- testLayerAndFeature
      let expr = wrap <$> (parseMaybe caseP "[\"case\",[\"<=\", 2, 1],\"x\",false,\"y\",\"otherwise\"]" :: Maybe (ArgType ('SString s)))
      (eval <$> expr <*> ctx) `shouldBe` Just (SString "otherwise")
  describe "Style.ExpressionsEval.eval" $ do
    it "interpolate expression linear" $ do
      ctx <- testLayerAndFeature
      let expr = wrap <$> (parseMaybe interpolateP "[\"interpolate\",[\"linear\"],5,0, 100, 10, 200]" :: Maybe (ArgType (SNum i)))
      (eval <$> expr <*> ctx) `shouldBe` Just (SNum (SDouble 150))
  describe "Style.ExpressionsEval.eval" $ do
    it "interpolate expression exponential" $ do
      ctx <- testLayerAndFeature
      let expr = wrap <$> (parseMaybe interpolateP "[\"interpolate\",[\"exponential\", 2],1,1,2,3,6]" :: Maybe (ArgType (SNum i)))
      (eval <$> expr <*> ctx) `shouldBe` Just (SNum (SDouble 2))
  describe "Style.ExpressionsEval.eval" $ do
    it "interpolate expression exponential '" $ do
      ctx <- testLayerAndFeature
      let expr = wrap <$> (parseMaybe interpolateP "[\"interpolate\",[\"exponential\", 2],2,1,2,3,6]" :: Maybe (ArgType (SNum i)))
      (eval <$> expr <*> ctx) `shouldBe` Just (SNum (SDouble 3.333333333333333))
  describe "Style.ExpressionsEval.eval" $ do
    it "less or eq expression" $ do
      ctx <- testLayerAndFeature
      let expr = wrap <$> parseMaybe ordP "[\"<=\", 2, 1]"
      (eval <$> expr <*> ctx) `shouldBe` Just (SBool False)
