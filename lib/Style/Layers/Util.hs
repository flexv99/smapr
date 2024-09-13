module Style.Layers.Util where

import qualified Data.Aeson as A
import qualified Data.Aeson.Text as A
import qualified Data.Aeson.Types as A
import Data.Text (toLower)
import Style.ExpressionsWrapper
import Style.IsoExpressions
import Style.Parser
import Text.Megaparsec

-- - Visible: The layer is shown.
-- - None: The layer is not shown.
-- defaults to Visible
data Visibility = Visible | None deriving (Enum, Eq, Show)

instance A.FromJSON Visibility where
  parseJSON = A.withText "Visibility" $ \t -> case toLower t of
    "visible" -> return Visible
    "none" -> return None
    _ -> return Visible

-- - Map: The line is translated relative to the map.
-- - Viewport: The line is translated relative to the viewport.
-- defaults to Map
data TranslateAnchor = Map | Viewport deriving (Enum, Eq, Show)

instance A.FromJSON TranslateAnchor where
  parseJSON = A.withText "LineTranslateAnchor" $ \t -> case toLower t of
    "map" -> return Map
    "viewport" -> return Viewport
    _ -> return Map

expr :: Maybe A.Value -> A.Parser (Maybe (IsoExpr INum))
expr Nothing = pure Nothing
expr (Just v) = case parse (try interpolateP <|> numExprP) "" (A.encodeToLazyText v) of
  Left err -> fail $ errorBundlePretty err
  Right res -> pure $ Just res

color :: Maybe A.Value -> A.Parser (Maybe Color)
color Nothing = pure Nothing
color (Just v) = case parse pColor "" (A.encodeToLazyText v) of
  Left err -> fail $ errorBundlePretty err
  Right res -> pure $ Just res
