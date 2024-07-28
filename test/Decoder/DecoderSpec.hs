{-# LANGUAGE OverloadedStrings #-}

module Decoder.DecoderSpec (spec) where

import Data.Functor ((<&>))
import qualified Data.Sequence as S
import qualified Data.Map as MP
import qualified Data.Text.Lazy as T
import Test.Hspec
import Data.Foldable
import Proto.Util
import Proto.Vector_tile.Tile.Layer
import ApiClient

spec :: Spec
spec = do
  describe "key value mapper" $ do
    it "map a feature's tags to the according keys and values" $ do
      (waterLayer >>= (\water -> return $ featureProperties <$> water <*> (head . toList . features <$> water))) `shouldReturn` featureTagsMapperRes
  describe "feature's geometry as string" $ do
    it "retrieve a feature's geometry type" $ do
      (waterLayer <&> fmap (geometryTypeToString . head . toList . features)) `shouldReturn` Just (Just "LINESTRING")
  describe "feature's id as string" $ do
    it "retrieve a feature's id" $ do
      (waterLayer <&> fmap (featureIdToString . head . toList . features)) `shouldReturn` Just (Just "1")
      

featureTagsMapperRes :: Maybe (MP.Map T.Text T.Text)
featureTagsMapperRes = Just (MP.fromList [("class","stream"),("intermittent","0"),("name_de",""),("name_en","")])
