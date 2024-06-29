{-# LANGUAGE DataKinds, DeriveGeneric, OverloadedStrings #-}

module Style.Poc where

import qualified Data.Aeson as A
import qualified Data.Aeson.Text as A
import qualified Data.Text.Lazy as T
import qualified Data.ByteString.Lazy.Internal as B
import Text.Megaparsec
import GHC.Generics
import Style.Parser

-- The goal of this proof of concept is to correctly parse the style of this water way
-- and apply this style to my test vector tile unsing Render.Geomety.renderLayer.

data POCLayer = POCLayer { id :: Int
                         , layerType :: T.Text
                         , source :: T.Text
                         , sourceLayer :: T.Text
                         , filter :: T.Text
                         , paint :: POCPaint
                         } deriving (Show, Eq, Generic)

data POCPaint = POCPaint { lineColor :: SColor Color
                         , lineOpacity :: SInteger
                         } deriving (Show, Eq, Generic)

instance A.FromJSON POCPaint where
  parseJSON = A.withObject "POCPaint" $ \ obj -> do
    lineColor   <- obj A..: "line-color" >>= parseSType
    lineOpacity <- obj A..: "line-opacity"
    return (POCPaint { lineColor = lineColor, lineOpacity = lineOpacity})
    where
      parseSType = A.withText "SType" $ \t ->
        case parse pHslColor "" (T.fromStrict t) of
          Left err  -> fail $ errorBundlePretty err
          Right res -> return res

tpaint :: B.ByteString
tpaint = "{\"line-color\": \"hsl(205, 56%, 73%)\", \"line-opacity\": 1}"

-- {
--   "id": "waterway",
--   "type": "line",
--   "source": "openmaptiles",
--   "source-layer": "waterway",
--   "filter": [
--     "all",
--     ["==", "$type", "LineString"],
--     ["!in", "brunnel", "tunnel", "bridge"]
--   ],
--   "paint": {
--     "line-color": "hsl(s205, 56%, 73%)",
--     "line-opacity": 1,
--     "line-width": {"base": 1.4, "stops": [[8, 1], [20, 8]]}
--   }
-- },


