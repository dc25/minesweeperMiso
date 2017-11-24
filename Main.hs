{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad.Random (Rand, getRandom, getRandomR, runRand)
import Control.Monad.State (State, get, put, runState)
import Data.Map (Map, (!), fromList, insert, mapWithKey, toList)
import System.Random

import Miso
import Miso.String (MisoString, ms, pack)
import Miso.Svg hiding (height_, style_, width_)

import Board
import Flag
import Mine
import Msg
import Pos
import RightClick
import Smiley

type Game = (Board, Int)

cellSize :: Int
cellSize = 20

getColor :: Cell -> String
getColor (Cell _ exposed _ _) =
    if exposed
        then "#909090"
        else "#AAAAAA"

showSquare :: Pos -> Cell -> View Msg
showSquare (xCoord, yCoord) cell =
    rect_
        [ x_ "0.05"
        , y_ "0.05"
        , width_ "0.9"
        , height_ "0.9"
        , style_ $ fromList [("fill", ms $ getColor cell)]
        , onClick (LeftPick (xCoord, yCoord))
        , onRightClick (RightPick (xCoord, yCoord))
        ]
        []

showText :: Pos -> Int -> [View Msg]
showText pos count =
    let textColor =
            case count of
                1 -> "cyan"
                2 -> "green"
                3 -> "red"
                4 -> "brown"
                _ -> "purple"
    in [ text_
             [ x_ "0.5"
             , y_ "0.87"
             , fontSize_ "1.0"
             , fill_ textColor
             , textAnchor_ "middle"
             , onClick (LeftPick pos)
             , onRightClick (RightPick pos)
             ]
             [text $ ms $ show count]
       ]

showCellDetail :: Pos -> Cell -> [View Msg]
showCellDetail pos (Cell mined exposed flagged mineCount) =
    case ( flagged,    mined, exposed, 0 /= mineCount) of
         (    True,       _,       _,       _) -> showFlag pos
         (       _,    True,    True,       _) -> showMine pos
         (       _,       _,    True,    True) -> showText pos mineCount
         (       _,       _,       _,       _) -> []

showCell :: Pos -> Cell -> View Msg
showCell pos cell =
    let (x, y) = pos
        scale = show cellSize
    in g_ [ transform_
                (ms $    "scale (" ++ scale ++ ", " ++ scale ++ ") " 
                      ++ "translate (" ++ show x ++ ", " ++ show y ++ ") ")
           ]
           (showSquare pos cell : showCellDetail pos cell)

centerStyle :: Map MisoString MisoString
centerStyle =
    fromList [("width", "75%"), ("margin", "0 auto"), ("text-align", "center")]

viewGame :: Game -> View Msg
viewGame (board, _) =
    div_
        []
        [ div_ [style_ centerStyle] (showFace (gameOver board))
        , div_
              [style_ centerStyle]
              [ svg_
                    [ version_ "1.1"
                    , width_ (ms $ show (w * cellSize))
                    , height_ (ms $ show (h * cellSize))
                    ]
                    (map snd (toList (mapWithKey showCell board)))
              ]
        , div_ [style_ centerStyle] [button_ [onClick Reset] [text "reset"]]
        ]

updateGame :: Msg -> Game -> Effect Msg Game
updateGame msg (board, seed) =
    case msg of
        Reset ->
            let g0 = mkStdGen seed
                (newBoard, g1) = runRand mkBoard g0
                (newSeed, _) = runRand getRandom g1
            in noEff (newBoard, newSeed)
        _ ->
            let (_, newBoard) = runState (updateBoard msg) board
            in noEff (newBoard, seed)

main :: IO ()
main = do
    seed <- getStdRandom random
    let initialAction = Reset
        model = (mempty, seed)
        update = updateGame
        view = viewGame
        events = Data.Map.insert "contextmenu" False defaultEvents
        subs = []
    startApp App {..}
