{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import System.Random
import Control.Monad.Random (Rand, runRand, getRandom, getRandomR)
import Control.Monad.State (State, runState, get, put)
import Data.Map (Map, mapWithKey, toList, fromList, insert, (!))

import           Miso
import           Miso.String  (MisoString, pack, ms)
import           Miso.Svg     hiding (height_, style_, width_)

import Pos
import Msg
import Smiley
import Mine
import Flag
import RightClick

data Cell = Cell { mined :: Bool 
                 , exposed :: Bool
                 , flagged :: Bool
                 , mineCount :: Int
                 } deriving (Show, Eq)

type Board = Map Pos Cell
type Game = (Board, Int)

w :: Int
w =  40

h :: Int
h = 30

cellSize :: Int
cellSize = 20

mkCell :: RandomGen g => Rand g Cell
mkCell = do
    t <- getRandomR (0.0::Float, 1.0)
    return $ Cell (t < 0.201) False False 0

initBoard :: RandomGen g => [Pos] -> Rand g Board
initBoard positions = do
    cells <- sequence $ take (length positions) (repeat mkCell)
    return $ fromList (zip positions cells)

mkBoard :: RandomGen g => Rand g Board
mkBoard = do
    let positions = [(x,y) | x <- [0..w-1], y <- [0..h-1]]   
    initBoard positions

getColor :: Cell -> String
getColor (Cell _ exposed _ _) = if exposed then "#909090" else "#AAAAAA"

showSquare :: Pos -> Cell -> View Msg
showSquare (xCoord, yCoord) cell = 
          rect_ [ x_ "0.05"
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
    let textColor = case count of
                        1 -> "cyan"
                        2 -> "green"
                        3 -> "red"
                        4 -> "brown"
                        _ -> "purple"

    in [ text_ [ x_ "0.5"
               , y_ "0.87" 
               , fontSize_ "1.0"
               , fill_ textColor
               , textAnchor_ "middle"
               , onClick (LeftPick pos)
               , onRightClick (RightPick pos)
               ] 
               [ text $ ms $ show count
               ]
       ]

showCellDetail :: Pos -> Cell -> [View Msg]
showCellDetail pos (Cell mined exposed flagged mineCount) = 
    case (  mined, exposed, flagged, 0 == mineCount) of
         (      _,       _,    True,     _) -> showFlag pos 
         (   True,    True,       _,     _) -> showMine pos 
         (      _,    True,       _, False) -> showText pos mineCount
         (      _,       _,       _,     _) -> []

showCell :: Pos -> Cell -> View Msg
showCell pos cell = 
    let (x,y) = pos 
    in g_ [ transform_ (ms $ "scale (" ++ show cellSize ++ ", " ++ show cellSize ++ ") " ++ "translate (" ++ show x ++ ", " ++ show y ++ ") " )
          ]
          ([ showSquare pos cell ] ++ showCellDetail pos cell)


adjacents :: Pos -> [Pos]
adjacents (x,y) = 
    [(xx,yy) | xx <- [x-1..x+1]
             , yy <- [y-1..y+1]
             , (xx,yy) /= (x,y)
             , xx >= 0, yy >= 0
             , xx < w, yy < h]

exposeMines :: State Board [(Pos, Maybe Cell)]
exposeMines = do
    board <- get
    let toExpose = filter (\(pos,cell) -> (not.exposed) cell && mined cell) $ toList board
        modifications = fmap (\(p,c) -> (p, Just $ c {exposed = True})) toExpose
    put $ foldl (\b (p,Just c) -> insert p c b) board modifications
    return modifications 

exposeSelection :: Pos -> Cell -> Int -> State Board [(Pos, Maybe Cell)]
exposeSelection pos cell count = do
    board <- get
    let cell = board ! pos
        toExpose = if flagged cell then [] else [(pos,cell)]
        modifications = fmap (\(p,c) -> (p, Just $ c {exposed = True, mineCount = count})) toExpose
    put $ foldl (\b (p,Just c) -> insert p c b) board modifications
    return modifications 
    
exposeCells :: Pos -> State Board [(Pos, Maybe Cell)]
exposeCells pos = do
    board <- get
    let cell@(Cell m e f mc) = board ! pos
        indices = adjacents pos
        count = length $ filter mined $ fmap (board !) indices
        checkList = if m || e || f || count /= 0 then [] else indices 

    exposedSelection <- exposeSelection pos cell count
    exposedNeighbors <- mapM exposeCells checkList 
    exposedMines <- if m then exposeMines else return []

    return $ exposedSelection ++ concat exposedNeighbors ++ exposedMines

updateBoard :: Msg -> State Board [(Pos, Maybe Cell)]
updateBoard (LeftPick pos) = exposeCells pos

updateBoard (RightPick pos ) = do
    board <- get
    let cell = board ! pos
        modifications = if exposed cell 
                        then [] -- can't flag a cell that's already exposed.  
                        else [(pos, Just $ cell {flagged=not $ flagged cell})]
    put $ foldl (\b (p,Just c) -> insert p c b) board modifications
    return modifications

gameOver :: Board -> Bool
gameOver = any (\cell -> exposed cell && mined cell) 

centerStyle :: Map MisoString MisoString
centerStyle = fromList [ ("width", "75%")
                       , ("margin", "0 auto")
                       , ("text-align", "center") 
                       ]

viewGame :: Game -> View Msg
viewGame (board,_) = 
    div_ []
    (   [div_ [style_ centerStyle] (showFace (gameOver board) )
        ,div_ [style_ centerStyle] [ svg_ [ version_ "1.1"
                                          , width_ (ms $ show (w * cellSize))
                                          , height_ (ms $ show (h * cellSize))
                                          ]
                      (map snd (toList (mapWithKey (\p c -> showCell p c) (board))))
                ]
        ,div_ [style_ centerStyle] [ button_ [onClick Reset] [text "reset"] ]
        ]
    )

updateGame :: Msg -> Game -> Effect Msg Game
updateGame msg (board, seed) = 
  case msg of
    Reset -> let g0 = mkStdGen seed
                 (newBoard,g1) = runRand mkBoard g0
                 (newSeed,_) = runRand getRandom g1
             in noEff (newBoard, newSeed)
    _ -> let (_,newBoard) = runState (updateBoard msg) board
         in noEff (newBoard, seed)

main :: IO ()
main = do
  seed <- getStdRandom random
  let
    initialAction = Reset
    model         = (mempty, seed)
    update        = updateGame
    view          = viewGame
    events        = defaultEvents
    subs          = [ ]
  startApp App {..}
