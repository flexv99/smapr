{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser.ParseSpec (spec) where

import Control.Monad.Except (runExceptT)
import Lens.Micro
import Style.Test.Unit
import Test.Hspec
import Text.Megaparsec

spec :: Spec
spec = do
  describe "coalesce" $ do
    it "coalesce basicl" $
      readTest "/test/json_test/coalesce/basic/test.json"
        >>= ( \test -> do
                res <- runExceptT $ runTestWithResult test
                let expecting = fmap (^. expected . outputs) test
                res `shouldBe` expecting
            )

-- "/test/json_test/coalesce/basic/test.json"
