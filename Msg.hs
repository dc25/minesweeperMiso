module Msg where 

import Control.Monad.Random (StdGen)

import Pos

data Msg = LeftPick Pos | RightPick Pos | Reset 
