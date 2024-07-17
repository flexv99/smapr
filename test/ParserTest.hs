{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Text.Megaparsec
import qualified Data.Text.Lazy as T
import Style.Parser
import Style.Expressions

main :: IO ()
main = hspec $ do
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
      parseMaybe pArray "[-1, 0, 0.4]" `shouldBe` Just (SArray [SInt (-1), SInt 0, SDouble 0.4])
  describe "Style.Parser.literal" $ do
    it "parse literals" $ do
      parseMaybe literal "[\"literal\", [\"a\", \"b\"]]" `shouldBe` Just expectedLiteralRes
  describe "Style.Expressions.getP" $ do
    it "can parse getters" $ do
      parseMaybe exprP "[\"get\",\"someProperyt\"]" `shouldBe` Just expectedGetRes
  describe "Style.Expressions.atP" $ do
    it "can parse at expression" $ do
      parseMaybe exprP "[\"at\", [\"literal\", [\"a\", \"b\", \"c\"]], 1]" `shouldBe` Just expectedAtRes
  describe "Style.Expressions.inP" $ do
    it "can parse in expression" $ do
      parseMaybe exprP "[\"in\", \"type\", \"Point\"]" `shouldBe` Just expectedInRes
  describe "Style.Expressions.indexOfP" $ do
    it "can parse index-of expressions with no starting index" $ do
      parseMaybe exprP "[\"index-of\", \"foo\", [\"baz\", \"bar\", \"hello\", \"foo\", \"world\"]]" `shouldBe` Just expectedIndexOfRes
  describe "Style.Expressions.indexOfP" $ do
    it "can parse index-of expressions with starting index" $ do
      parseMaybe exprP "[\"index-of\", \"foo\", [\"baz\", \"bar\", \"hello\", \"foo\", \"world\"], 2]" `shouldBe` Just expectedIndexOfRes'
  describe "Style.Expressions.eqP" $ do
    it "can parse eqality expression" $ do
      parseMaybe exprP "[\"==\",\"$type\",\"LineString\"]" `shouldBe` Just expectedEq
  describe "Style.Expressions.eqP" $ do
    it "can parse eqality expression on literal" $ do
      parseMaybe exprP "[\"==\",1, 1]" `shouldBe` Just expectedEqOnSType

expectedGetRes :: Expression
expectedGetRes = SGetType $ SGet (SString "someProperyt")

expectedAtRes :: Expression
expectedAtRes = SAtType $ SAt {array = SArray [SString "a",SString "b",SString "c"], index = SInt 1}

expectedLiteralRes :: SType
expectedLiteralRes = SArray [SString "a", SString "b"]

expectedInRes :: Expression
expectedInRes = SInType $ SIn {object = SString "type", item = SString "Point"}

expectedIndexOfRes :: Expression                
expectedIndexOfRes = SIndexOfType $ SIndexOf {lookupItem = SString "foo", items = SArray [SString "baz",SString "bar",SString "hello",SString "foo",SString "world"]
                              , startIndex = Nothing}

expectedIndexOfRes' :: Expression
expectedIndexOfRes' = SIndexOfType $ SIndexOf {lookupItem = SString "foo", items = SArray [SString "baz",SString "bar",SString "hello",SString "foo",SString "world"]
                               , startIndex = Just (SInt 2)}

expectedEq :: Expression
expectedEq = SEqType $ SEq { iOne = STypeType "$type", iTwo = SString "LineString" }

expectedEqOnSType :: Expression
expectedEqOnSType = SEqType $ SEq { iOne = SInt 1, iTwo = SInt 1 }

waterLayerStyle :: T.Text
waterLayerStyle = "{\"id\":\"waterway\",\"type\":\"line\",\"source\":\"openmaptiles\",\"source-layer\":\"waterway\",\"filter\":[\"all\",[\"==\",\"$type\",\"LineString\"],[\"!in\",\"brunnel\",\"tunnel\",\"bridge\"],[\"!=\",\"intermittent\",1]],\"layout\":{\"visibility\":\"visible\"},\"paint\":{\"line-color\":\"hsl(205,56%,73%)\",\"line-opacity\":1,\"line-width\":{\"base\":1.4,\"stops\":[[8,1],[20,8]]}}}"
