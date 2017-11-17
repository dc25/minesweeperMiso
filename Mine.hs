{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Mine (showMine) where

import           Miso
import           Miso.String        (MisoString, pack, ms)
import           Miso.Svg           hiding (height_, id_, style_, width_)

import RightClick


import Data.Map (fromList)

import Pos
import Msg

showMine :: Pos -> [View Msg]
showMine pos = 
    [ polygon_ [ points_ "0.65,0.15 0.85,0.35 0.65,0.55 0.45,0.35 " 
               , fill_   "brown"
               ]
               [
               ]

    , circle_ [ cx_ "0.45" 
              , cy_ "0.55"
              , r_  "0.3"
              , fill_ "brown"
              , onClick (LeftPick pos)
              , onRightClick (RightPick pos)
             ] 
             [
             ]
    ]
