import Test.Hspec
import Text.Megaparsec
import Style.Parser
import Style.Expressions
import qualified Data.Text.Lazy as T
import qualified Data.Aeson.Text as A

main :: IO ()
main = hspec $ do
  describe "Style.Expressions.getP" $ do
    it "can parse getters" $ do
      parseTest getP (A.encodeToLazyText ["get", "someProperyt"])
  describe "Style.Parser.literal" $ do
    it "parse literals" $ do
      parseMaybe literal (T.pack "[\"literal\", [\"a\", \"b\"]]") `shouldBe` (Just (map T.pack ["a", "b"]))
