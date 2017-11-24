module Board where

import Control.Monad.Random
    ( Rand
    , RandomGen
    , getRandomR
    , getStdGen
    , runRand
    , split
    )
import Control.Monad.State
import Data.Map (Map, (!), elems, fromList, insert, toList)

import Msg
import Pos

w :: Int
w = 32

h :: Int
h = 16

data Cell = Cell
    { mined :: Bool
    , exposed :: Bool
    , flagged :: Bool
    , mineCount :: Int
    } deriving (Show, Eq)

type Board = Map Pos Cell

mkCell :: RandomGen g => Rand g Cell
mkCell = do
    t <- getRandomR (0.0 :: Float, 1.0)
    return $ Cell (t < 0.201) False False 0

initBoard :: RandomGen g => [Pos] -> Rand g Board
initBoard positions = do
    cells <- sequence $ take (length positions) (repeat mkCell)
    return $ fromList (zip positions cells)

mkBoard :: RandomGen g => Rand g Board
mkBoard = do
    let positions = [(x, y) | x <- [0 .. w - 1], y <- [0 .. h - 1]]
    initBoard positions

adjacents :: Pos -> [Pos]
adjacents (x, y) =
    [ (xx, yy)
    | xx <- [x - 1 .. x + 1]
    , yy <- [y - 1 .. y + 1]
    , (xx, yy) /= (x, y)
    , xx >= 0
    , yy >= 0
    , xx < w
    , yy < h
    ]

exposeMines :: State Board [(Pos, Maybe Cell)]
exposeMines = do
    board <- get
    let toExpose =
            filter (\(pos, cell) -> (not . exposed) cell && mined cell) $
            toList board
        modifications =
            fmap (\(p, c) -> (p, Just $ c {exposed = True})) toExpose
    put $ foldl (\b (p, Just c) -> insert p c b) board modifications
    return modifications

exposeSelection :: Pos -> Cell -> Int -> State Board [(Pos, Maybe Cell)]
exposeSelection pos cell count = do
    board <- get
    let cell = board ! pos
        toExpose =
            if flagged cell
                then []
                else [(pos, cell)]
        modifications =
            fmap
                (\(p, c) -> (p, Just $ c {exposed = True, mineCount = count}))
                toExpose
    put $ foldl (\b (p, Just c) -> insert p c b) board modifications
    return modifications

exposeCells :: Pos -> State Board [(Pos, Maybe Cell)]
exposeCells pos = do
    board <- get
    let cell@(Cell m e f mc) = board ! pos
        indices = adjacents pos
        count = length $ filter mined $ fmap (board !) indices
        checkList =
            if m || e || f || count /= 0
                then []
                else indices
    exposedSelection <- exposeSelection pos cell count
    exposedNeighbors <- mapM exposeCells checkList
    exposedMines <-
        if m
            then exposeMines
            else return []
    return $ exposedSelection ++ concat exposedNeighbors ++ exposedMines

flagCell :: Pos -> State Board [(Pos, Maybe Cell)]
flagCell pos = do
    board <- get
    let cell = board ! pos
        modifications =
            if exposed cell
                then [] -- can't flag a cell that's already exposed.  
                else [(pos, Just $ cell {flagged = not $ flagged cell})]
    put $ foldl (\b (p, Just c) -> insert p c b) board modifications
    return modifications

gameOver :: Board -> Bool
gameOver = any (\cell -> exposed cell && mined cell)

updateBoard :: Msg -> State Board [(Pos, Maybe Cell)]
updateBoard msg = do
    board <- get
    if gameOver board
        then return []
        else case msg of
                 LeftPick pos -> exposeCells pos
                 RightPick pos -> flagCell pos
