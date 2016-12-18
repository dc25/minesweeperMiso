{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Mine (showMine) where

import Reflex
import Reflex.Dom
import Data.Map (fromList)

import Svg
import Pos

showMine :: MonadWidget t m => Pos -> m [El t]
showMine pos = do
    let mineAttrs = 
            fromList [ ( "cx", "0.45" )
                     , ( "cy", "0.55" )
                     , ( "r",  "0.3" )
                     , ( "style",        "fill:brown")
                     , ("oncontextmenu", "return false;")
                     ] 

    (cEl,_) <- elSvgns "circle" (constDyn mineAttrs ) $ return ()

    let stemAttrs = 
            fromList [ ( "points", "0.65,0.15 0.85,0.35 0.65,0.55 0.45,0.35 " )
                     , ( "style",        "fill:brown")
                     , ("oncontextmenu", "return false;")
                     ] 

    (sEl,_) <- elSvgns "polygon" (constDyn stemAttrs ) $ return ()
    (fEl,_) <- elSvgns "circle" (constDyn mineAttrs ) $ return ()

    return [cEl, sEl]
