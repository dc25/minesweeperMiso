{-# LANGUAGE OverloadedStrings #-}
module Smiley ( showFace) where

import           Miso
import           Miso.String        (pack)
import           Miso.Svg           hiding (height_, id_, style_, width_)

import Msg

showFace :: Bool -> [View Msg]
showFace lost = 
  let sz = 100
  in [ svg_ [ width_ "100" , height_ "100" ] 
            [ g_ [ transform_ (pack $ "scale (" ++ show sz ++ ", " ++ show sz ++ ") " ++ "translate (0.5, 0.5)") ] 
                 [ -- face outline
                   circle_ [ cx_ "0.0"
                           , cy_ "0.0"
                           , r_  "0.4"
                           , stroke_ "black"
                           , strokeWidth_ "0.02"
                           ]
                           [
                           ]
                 ]
           ]
     ]
