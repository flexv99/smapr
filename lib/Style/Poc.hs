{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}

module Style.Poc where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Internal as B
import qualified Data.Text.Lazy as T
import qualified Data.Sequence as S
import GHC.Generics
import Style.Parser
import Style.ExpressionsWrapper
import Style.ExpressionsEval
import Style.ExpressionsContext
import ApiClient
import Proto.Vector_tile.Tile.Layer (Layer(..))
import Proto.Vector_tile.Tile (Tile(..))
import Proto.Util
import Style.Layers.Wrapper


-- The goal of this proof of concept is to correctly parse the style of this water way
-- and apply this style to my test vector tile unsing Render.Geomety.renderLayer.

data Width = Width
  { base :: Maybe SType
  , stops :: Maybe SType
  } deriving (Show, Eq, Generic)

-- Helper for use in combination with .:? to provide default values for optional JSON object fields.

instance A.FromJSON Width where
  parseJSON = A.withObject "Width" $ \obj ->
    Width
      <$> obj A..:? "base"
      <*> obj A..:? "stops"



waterLayerStyle :: B.ByteString
waterLayerStyle = "{\"id\":\"waterway\",\"type\":\"line\",\"source\":\"openmaptiles\",\"source-layer\":\"waterway\",\"filter\":[\"all\",[\"==\",[\"geometry-type\"],\"LineString\"],[\"match\",[\"get\",\"brunnel\"],[\"bridge\",\"tunnel\"],false,true],[\"!=\",[\"get\",\"intermittent\"],1]],\"layout\":{\"visibility\":\"visible\"},\"paint\":{\"line-color\":\"hsl(205,56%,73%)\",\"line-opacity\":1,\"line-width\":[\"interpolate\",[\"exponential\",1.4],[\"zoom\"],8,1,20,8]}}"

transportationLayerStyle :: B.ByteString
transportationLayerStyle = "{\"id\":\"road_trunk_primary\",\"type\":\"line\",\"source\":\"openmaptiles\",\"source-layer\":\"transportation\",\"filter\":[\"all\",[\"==\",[\"geometry-type\"],\"LineString\"],[\"match\",[\"get\",\"class\"],[\"primary\",\"trunk\"],true,false]],\"layout\":{\"line-cap\":\"round\",\"line-join\":\"round\"},\"paint\":{\"line-color\":\"#fff\",\"line-width\":[\"interpolate\",[\"exponential\",1.4],[\"zoom\"],6,0.5,20,30]}}"

evalTester :: Maybe WrappedExpr -> IO (Maybe SType)
evalTester expr =
  testLayerAndFeature >>= (\ctx -> return (eval <$> expr <*> ctx))

evalLayer :: SLayer -> ExpressionContext -> SType
evalLayer (SLayer {lfilter = fltr}) = eval $ wrap fltr

constructCtx :: S.Seq Layer -> S.Seq ExpressionContext
constructCtx (l S.:<| xs) = create l S.>< constructCtx xs
  where
    create :: Layer -> S.Seq ExpressionContext
    create l' = fmap (\f -> ExpressionContext f l' 14 ) (features l')
constructCtx S.Empty      = S.empty

toBeDrawn :: Tile -> SLayer -> S.Seq ExpressionContext
toBeDrawn t s = fmap (S.filter (unwrapSBool . evalLayer s)) constructCtx layers'
  where
   layers' = getLayers (T.unpack $ sourceLayer s) t

-- test :: IO ()
-- test = do
--   t <- fakerTile
--   let stile   = A.decode transportationLayerStyle :: Maybe SLayer
--   let tbD     = toBeDrawn <$> t <*> stile
--   let diagram = D.bg D.lightblue <$> ((renderLayer . paint <$> stile) <*> tbD)
--   maybe (putStrLn "Nothing") writeSvg diagram
