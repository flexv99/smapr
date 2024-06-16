{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Text.Megaparsec
import Style.Parser
import Style.Expressions

main :: IO ()
main = hspec $ do
  describe "Style.Types.Stypes.SBool" $ do
    it "parse bool" $ do
      parseMaybe pAtom "false" `shouldBe` (Just (SBool False))
  describe "Style.Types.Stypes.SNumber" $ do
    it "parse number" $ do
      parseMaybe pAtom "1" `shouldBe` (Just (SInteger 1))
  describe "Style.Types.Stypes.SNumber" $ do
    it "parse signed number" $ do
      parseMaybe pAtom "-1" `shouldBe` (Just (SInteger (-1)))
  describe "Style.Types.Stypes.SNumber" $ do
    it "parse float" $ do
      parseMaybe pAtom "1.001" `shouldBe` (Just (SDouble 1.001))
  describe "Style.Types.Stypes.SString" $ do
    it "parse string" $ do
      parseMaybe pAtom "\"abc\"" `shouldBe` (Just (SString "abc"))
  describe "Style.Types.Stypes.SString" $ do
    it "parse snake cased string" $ do
      parseMaybe pAtom "\"ab_c\"" `shouldBe` (Just (SString "ab_c"))
  describe "Style.Types.Stypes.SType" $ do
    it "parse array" $ do
      parseMaybe pArray "[-1, 0, 0.4]" `shouldBe` (Just ([SInteger (-1), SInteger 0, SDouble 0.4]))
  describe "Style.Expressions.getP" $ do
    it "can parse getters" $ do
      parseMaybe getP "[\"get\",\"someProperyt\"]" `shouldBe` (Just expectedGetRes)
  describe "Style.Expressions.atP" $ do
    it "can parse at expression" $ do
      parseMaybe atP "[\"at\", [\"literal\", [\"a\", \"b\", \"c\"]], 1]" `shouldBe` (Just expectedAtRes)
  describe "Style.Parser.literal" $ do
    it "parse literals" $ do
      parseMaybe literal "[\"literal\", [\"a\", \"b\"]]" `shouldBe` (Just expectedLiteralRes)


expectedGetRes :: SGet
expectedGetRes = SGet (SString "someProperyt")

expectedAtRes :: SAt SType
expectedAtRes = SAt {array = [SString "a",SString "b",SString "c"], index = 1}

expectedLiteralRes :: [SType]
expectedLiteralRes = [SString "a", SString "b"]
