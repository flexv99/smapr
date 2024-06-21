module Style.Poc where

import qualified Data.Aeson.Text as A

-- The goal of this proof of concept is to correctly parse the style of this water way
-- and apply this style to my test vector tile unsing Render.Geomety.renderLayer.

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
--     "line-color": "hsl(205, 56%, 73%)",
--     "line-opacity": 1,
--     "line-width": {"base": 1.4, "stops": [[8, 1], [20, 8]]}
--   }
-- },


