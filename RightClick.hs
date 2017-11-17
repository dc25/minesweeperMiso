{-# LANGUAGE OverloadedStrings #-}

module RightClick (onRightClick) where

import Miso.Event.Decoder
import Miso.Html

onRightClick :: msg -> Attribute msg
onRightClick message =
  onWithOptions
    Options { preventDefault = True, stopPropagation = True }
    "contextmenu"
    emptyDecoder
    (const message)
