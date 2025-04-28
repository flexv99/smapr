module Style.Layers.Point where

import qualified Data.Aeson as A

data PointS = PointS {} deriving (Show)

makeLenses ''PointS

instance A.FromJSON PointS 
