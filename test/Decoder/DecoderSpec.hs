module Decoder.DecoderSpec where

import qualified Data.Sequence as S
import qualified Data.Map as MP
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
      app `shouldReturn` featureTagsMapperRes

featureTagsMapperRes :: Maybe [MP.Map String String]
featureTagsMapperRes = Just [MP.fromList [("collision_rank","river"),("id","41431201"),("kind","stream"),("min_zoom","11.0"),("name","512"),("name:de","Gran Ega"),("name:it","Gader Bach"),("sort_rank","201"),("source","openstreetmap.org")],MP.fromList [("collision_rank","river"),("id","41431201"),("kind","stream"),("min_zoom","11.0"),("name","512"),("name:de","Gran Ega"),("name:it","Gader Bach"),("sort_rank","201"),("source","openstreetmap.org")],MP.fromList [("collision_rank","river"),("id","41431201"),("kind","stream"),("min_zoom","11.0"),("name","512"),("name:de","Gran Ega"),("name:it","Gader Bach"),("sort_rank","201"),("source","openstreetmap.org")]]
