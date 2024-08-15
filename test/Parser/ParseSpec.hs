{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

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
      fmap showSColor (parseMaybe pAtom "hsl(205,56%,73%)") `shouldBe` Just "#94c1e1"
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


waterLayerStyle :: T.Text
waterLayerStyle = "{\"version\":8,\"name\":\"Basic\",\"metadata\":{\"mapbox:autocomposite\":false,\"mapbox:type\":\"template\",\"maputnik:renderer\":\"mbgljs\",\"openmaptiles:version\":\"3.x\",\"openmaptiles:mapbox:owner\":\"openmaptiles\",\"openmaptiles:mapbox:source:url\":\"mapbox://openmaptiles.4qljc88t\"},\"sources\":{\"openmaptiles\":{\"type\":\"vector\",\"url\":\"https://api.maptiler.com/tiles/v3-openmaptiles/tiles.json?key={key}\"}},\"sprite\":\"https://openmaptiles.github.io/maptiler-basic-gl-style/sprite\",\"glyphs\":\"https://api.maptiler.com/fonts/{fontstack}/{range}.pbf?key={key}\",\"layers\":[{\"id\":\"water\",\"type\":\"fill\",\"source\":\"openmaptiles\",\"source-layer\":\"water\",\"filter\":[\"all\",[\"==\",[\"geometry-type\"],\"Polygon\"],[\"!=\",[\"get\",\"intermittent\"],1],[\"!=\",[\"get\",\"brunnel\"],\"tunnel\"]],\"layout\":{\"visibility\":\"visible\"},\"paint\":{\"fill-color\":\"hsl(205,56%,73%)\"}}],\"id\":\"basic\"}"
