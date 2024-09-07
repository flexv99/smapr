{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE KindSignatures            #-}

module Style.Layers.Wrapper where

import qualified Data.Text.Lazy as T
import qualified Data.Aeson as A
import qualified Data.Aeson.Text as A
import Control.Lens
import Text.Megaparsec
import Style.Layers.Line
import Style.Layers.Fill
import Style.ExpressionsWrapper
import Style.IsoExpressions
import Style.Parser

data Paint = LinePaint LineS | FillPaint FillS deriving (Show)

data SLayer = forall (b :: Bool). SLayer
  { id          :: T.Text
  , pType       :: T.Text
  , source      :: T.Text
  , sourceLayer :: T.Text
  , lfilter     :: Maybe (ArgType ('SBool b))
  , paint       :: Paint
  }
makeLenses ''SLayer

deriving instance Show SLayer


instance A.FromJSON SLayer where
  parseJSON = A.withObject "POCLayer" $ \obj -> do
      id'          <- obj A..: "id"
      type'        <- obj A..: "type"
      source'      <- obj A..: "source"
      sourceLayer' <- obj A..: "source-layer"
      filter'      <- obj A..:? "filter" >>= (\p -> return $ fexpr <$> p)
      p            <- obj A..: "paint"
      paint'       <- paintP type' p
      return $ SLayer id' type' source' sourceLayer' filter' paint'
    where
      fexpr = A.withArray "FilterExpression" (\v ->
        case parse allP "" (A.encodeToLazyText v) of
          Left err  -> fail $ errorBundlePretty err
          Right res -> pure res)
      paintP t = A.withObject "Paint" $ \v -> do
         case t of
           "line"  -> LinePaint <$> A.parseJSON (A.Object v)
           "fill"  -> FillPaint <$> A.parseJSON (A.Object v)
