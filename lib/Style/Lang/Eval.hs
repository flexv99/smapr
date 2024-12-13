{-# LANGUAGE MonoLocalBinds #-}

module Style.Lang.Eval where

import Data.Scientific
import Data.Text.Lazy as T
import Style.Lang.Ast
import Style.Lang.Parser
import Style.Lang.Types

eval :: SExpr a -> a
eval (NumE i) = i
eval (StringE s) = s
eval (TextAtE t i) =
  fmap
    T.singleton
    ( T.index
        <$> eval t
        <*> (floor . toRealFloat <$> eval i)
    )
eval _ = error "not yet implemented"

--------------------------------------------------------------------------------
