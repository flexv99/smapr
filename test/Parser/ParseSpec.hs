{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser.ParseSpec (spec) where

import qualified Data.Text.Lazy as T
import Proto.Util
import Style.ExpressionsWrapper
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
      fmap showSColor (parseMaybe pColor "\"hsl(205,56%,73%)\"") `shouldBe` Just "#94c1e1"
  describe "Style.Parser.SType.SColor" $ do
    it "parse rgba color" $ do
      fmap showSColor (parseMaybe pColor "\"rgba(179,222,155,0.6)\"") `shouldBe` Just "#b3de9b"
  describe "Style.Parser.SType.SColor" $ do
    it "parse rgb color" $ do
      fmap showSColor (parseMaybe pColor "\"rgb(183,220,163)\"") `shouldBe` Just "#b7dca3"
  describe "Style.Parser.SType.SColor" $ do
    it "parse hex color" $ do
      fmap showSColor (parseMaybe pColor "\"#00aaff\"") `shouldBe` Just "#00aaff"
  describe "Style.Parser.SType.SColor" $ do
    it "parse short hex color" $ do
      fmap showSColor (parseMaybe pColor "\"#0af\"") `shouldBe` Just "#00aaff"
  describe "Style.Parser.SType.SArray" $ do
    it "parse array" $ do
      parseMaybe pAtom "[-1, 0, 4]" `shouldBe` Just (SArray $ map SNum [SInt (-1), SInt 0, SInt 4])
  describe "Style.Parser.SType.SNull" $ do
    it "parse null" $ do
      parseMaybe pAtom "null" `shouldBe` Just SNull
  describe "Style.Expressions.sumP" $ do
    it "can parse and evaluate sum expression" $ do
      ctx <- testLayerAndFeature
      (eval <$> parseMaybe numExprP "[\"+\", 1, 2, 3, [\"-\", 1, 2]]" <*> ctx) `shouldBe` Just (SInt 5)
  describe "Style.Expressions.prodP" $ do
    it "can parse and evaluate multiplication expression" $ do
      ctx <- testLayerAndFeature
      (eval <$> parseMaybe numExprP "[\"*\", 1, [\"-\", 1, 2], 5, 6]" <*> ctx) `shouldBe` Just (SInt (-30))
  describe "Style.Expressions.subP" $ do
    it "can parse and evaluate subtraction expression" $ do
      ctx <- testLayerAndFeature
      (eval <$> parseMaybe numExprP "[\"-\", 1, [\"+\", 1, 2]]" <*> ctx) `shouldBe` Just (SInt (-2))
  describe "Style.Expressions.divP" $ do
    it "can parse and evaluate division expression" $ do
      ctx <- testLayerAndFeature
      (eval <$> parseMaybe numExprP "[\"/\", 1, [\"+\", 2, 2]]" <*> ctx) `shouldBe` Just (SDouble 0.25)
  describe "Style.Expressions.eqP" $ do
    it "can parse and evaluate eqality expression" $ do
      ctx <- testLayerAndFeature
      (eval <$> parseMaybe eqP "[\"==\",1, 1]" <*> ctx) `shouldBe` Just True
  describe "Style.Expressions.atP" $ do
    it "can parse and return element at index of a list" $ do
      ctx <- testLayerAndFeature
      (eval <$> parseMaybe stringExprP "[\"at\", [\"a\", \"bc\", \"tre\"], 1]" <*> ctx) `shouldBe` Just "bc"
  describe "Style.Expressions.atP" $ do
    it "at on ctx related list" $ do
      ctx <- testLayerAndFeature
      (eval <$> parseMaybe stypeExprP "[\"at\", [[\"get\",\"intermittent\"], [\"get\",\"tunnel\"]], 0]") <*> ctx `shouldBe` Just (SNum (SInt 0))
  describe "Style.Expressions.inP" $ do
    it "can parse and evaluate in expressions" $ do
      ctx <- testLayerAndFeature
      (eval <$> parseMaybe inP "[\"in\", 1, [9, 8, 7]]" <*> ctx) `shouldBe` Just False
  describe "Style.Expressions.inP" $ do
    it "can parse and evaluate in expressions" $ do
      ctx <- testLayerAndFeature
      (eval <$> parseMaybe inP "[\"in\", \"abc\", \"abcz\"]" <*> ctx) `shouldBe` Just True
  describe "Style.FilterExpressions.fgeometryP" $ do
    it "parse geometry type function that retrieves a feature's geometry" $ do
      ctx <- testLayerAndFeature
      let expr = parseMaybe fgeometryP "[\"geometry-type\"]"
      (eval <$> expr <*> ctx) `shouldBe` Just "LINESTRING"
  describe "Style.FilterExpressions.evalFilterGetter" $ do
    it "can evaluate a in getter Expression on feature properties" $ do
      ctx <- testLayerAndFeature
      let expr = parseMaybe fgetP "[\"get\",\"intermittent\"]" :: Maybe (IsoExpr SType)
      (eval <$> expr <*> ctx) `shouldBe` Just (SNum $ SInt 0)
  describe "Style.ExpressionsEval.eval" $ do
    it "combination of feature and iso expressions" $ do
      ctx <- testLayerAndFeature
      let expr = parseMaybe eqP "[\"==\", [\"geometry-type\"], \"LINESTRING\"]"
      (eval <$> expr <*> ctx) `shouldBe` Just True
  describe "Style.ExpressionsEval.eval" $ do
    it "combination of feature and iso expressions '" $ do
      ctx <- testLayerAndFeature
      let expr = parseMaybe eqP "[\"!=\",[\"get\",\"intermittent\"], 1]"
      (eval <$> expr <*> ctx) `shouldBe` Just True
  describe "Style.ExpressionsEval.eval" $ do
    it "combination of feature and iso expressions ''" $ do
      ctx <- testLayerAndFeature
      let expr = parseMaybe eqP "[\"==\",[\"get\",\"intermittent\"],0]"
      (eval <$> expr <*> ctx) `shouldBe` Just True
  describe "Style.ExpressionsEval.eval" $ do
    it "match expression" $ do
      ctx <- testLayerAndFeature
      let expr = (parseMaybe (matchP (\(SBool b) -> b)) "[\"match\", [\"get\", \"brunnel\"], [\"bridge\", \"tunnel\"], false, true]" :: Maybe (IsoExpr Bool))
      (eval <$> expr <*> ctx) `shouldBe` Just True
  describe "Style.ExpressionsEval.eval" $ do
    it "case expression" $ do
      ctx <- testLayerAndFeature
      let expr = (parseMaybe caseP "[\"case\",[\"<=\", 2, 1],\"x\",false,\"y\",\"otherwise\"]" :: Maybe (IsoExpr T.Text))
      (eval <$> expr <*> ctx) `shouldBe` Just "otherwise"
  describe "Style.ExpressionsEval.eval" $ do
    it "coalesce expression" $ do
      ctx <- testLayerAndFeature
      let expr = (parseMaybe coalesceP "[\"coalesce\",[\"get\", \"brunnel\"],[\"get\", \"tunnel\"],0]" :: Maybe (IsoExpr SType))
      (eval <$> expr <*> ctx) `shouldBe` Just (SNum $ SInt 0)
  describe "Style.ExpressionsEval.eval" $ do
    it "interpolate expression linear" $ do
      ctx <- testLayerAndFeature
      let expr = (parseMaybe interpolateNumP "[\"interpolate\",[\"linear\"],5,0, 100, 10, 200]" :: Maybe (IsoExpr INum))
      (eval <$> expr <*> ctx) `shouldBe` Just (SDouble 150)
  describe "Style.ExpressionsEval.eval" $ do
    it "interpolate expression exponential" $ do
      ctx <- testLayerAndFeature
      let expr = (parseMaybe interpolateNumP "[\"interpolate\",[\"exponential\", 2],1,1,2,3,6]" :: Maybe (IsoExpr INum))
      (eval <$> expr <*> ctx) `shouldBe` Just (SDouble 2)
  describe "Style.ExpressionsEval.eval" $ do
    it "interpolate expression exponential '" $ do
      ctx <- testLayerAndFeature
      let expr = (parseMaybe interpolateNumP "[\"interpolate\",[\"exponential\", 2],2,1,2,3,6]" :: Maybe (IsoExpr INum))
      (eval <$> expr <*> ctx) `shouldBe` Just (SDouble 3.333333333333333)
  describe "Style.ExpressionsEval.eval" $ do
    it "less or eq expression" $ do
      ctx <- testLayerAndFeature
      let expr = parseMaybe ordP "[\"<=\", 2, 1]"
      (eval <$> expr <*> ctx) `shouldBe` Just False
