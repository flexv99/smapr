{-# LANGUAGE OverloadedStrings #-}

module Renderer.Lines where

import Graphics.Svg
import Renderer.Helper

svg :: Element -> Element
svg content =
  doctype
  <> with (svg11_ content) [Version_ <<- "1.1", Width_ <<- "340", Height_ <<- "200"]

contents :: Element
contents =
  path_ [ Fill_ <<- "#352950"
        , D_    <<- ( mA 0 340 <> lA 113 170 <> lA 0 0 <> lA 85 0
                      <> lA 198 170 <> lA 85 340 <> lA 0 340 <> z <> mA 0 340 ) ]

