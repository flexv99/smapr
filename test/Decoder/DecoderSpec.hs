module Decoder.DecoderSpec where

import qualified Data.Sequence as S
import Data.Functor ((<&>))
import Test.Hspec
import Decoder.FeatureAttributes
import Proto.Vector_tile.Tile.Layer
import ApiClient

waterLayer :: IO (Maybe Layer)
waterLayer = fakerTile <&> fmap (\l -> getLayers "water" l `S.index` 0)

decodeSpec :: Spec
decodeSpec = do
  describe "key value mapper" $ do
    it "map a feature's tags to the according keys and values" $ do
      let app = waterLayer <&> fmap featureProperties
      app `shouldReturn` Just []
