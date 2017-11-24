{-# LANGUAGE OverloadedStrings #-}

module Flag
    ( showFlag
    ) where

import Miso
import Miso.String (MisoString, ms, pack)
import Miso.Svg hiding (height_, id_, style_, width_)

import Msg
import Pos
import RightClick

showFlag :: Pos -> [View Msg]
showFlag pos =
    [ polygon_
          [ points_ "0.20,0.40 0.70,0.55 0.70,0.25"
          , fill_ "red"
          , onClick (LeftPick pos)
          , onRightClick (RightPick pos)
          ]
          []
    , line_
          [ x1_ "0.70"
          , y1_ "0.25"
          , x2_ "0.70"
          , y2_ "0.85"
          , strokeWidth_ ".07"
          , stroke_ "black"
          , onClick (LeftPick pos)
          , onRightClick (RightPick pos)
          ]
          []
    ]
