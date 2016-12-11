{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Reflex
import Reflex.Dom
import Control.Monad.Random (RandomGen, Rand, runRand, getStdGen, getRandomR)
import Control.Monad.Trans (liftIO)
import Control.Monad.State (State, state, runState, get, put)
import Data.Map as DM (Map, toList, fromList, elems, lookup, findWithDefault, insert, mapWithKey, (!))
import Data.Text as DT (Text, pack, append)
import Data.Functor.Misc (dmapToMap, mapWithFunctorToDMap)
import Data.Traversable (forM)

data Cell = Cell { mined :: Bool 
                 , exposed :: Bool
                 , flagged :: Bool
                 , mines :: Int
                 } deriving Show

type Pos = (Int, Int)
type Board = Map Pos Cell

data Msg = LeftPick Pos | RightPick Pos 

w :: Int
w =  16

h :: Int
h = 16

cellSize :: Int
cellSize = 20

mkCell :: RandomGen g => Rand g Cell
mkCell = do
    t <- getRandomR (0.0::Float, 1.0)
    return $ Cell (t < 0.201) False False 0

initBoard :: RandomGen g => [Pos] -> Rand g Board
initBoard positions = do
    cells <- sequence $ repeat mkCell
    return $ fromList (zip positions cells)

mkBoard :: RandomGen g => Rand g Board
mkBoard = do
    let positions = [(x,y) | x <- [0..w-1], y <- [0..h-1]]   
    initBoard positions

getColor :: Cell -> String
getColor (Cell _ exposed _ _) = if exposed then "#909090" else "#AAAAAA"

cellAttrs :: Cell -> Map Text Text
cellAttrs cell =
    fromList [ ( "x",            "0.05")
             , ( "y",            "0.05")
             , ( "width",        "0.9")
             , ( "height",       "0.9")
             , ( "style",        pack $ "fill:" ++ getColor cell)
             , ( "oncontextmenu", "return false;")
             ]

textAttrs :: Map Text Text
textAttrs = 
    fromList [ ("x",             "0.5")
             , ("y",             "0.6")
             , ("font-size",     "1.0" )
             , ("fill",          "blue" )
             , ("alignment-baseline", "middle" )
             , ("text-anchor",        "middle" )
             , ("oncontextmenu",      "return false;")
             ] 

groupAttrs :: Pos -> Map Text Text
groupAttrs (x,y) = 
    fromList [ ("transform", 
                pack $    "scale (" ++ show cellSize ++ ", " ++ show cellSize ++ ") " 
                       ++ "translate (" ++ show x ++ ", " ++ show y ++ ")" 
               )
             ] 

showSquare :: MonadWidget t m => Cell -> m [El t]
showSquare cell = do
    (rEl,_) <- elSvgns "rect" (constDyn $ cellAttrs cell) $ return ()
    return [rEl]

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

showFlag :: MonadWidget t m => Pos -> m [El t]
showFlag pos = do
    let flagAttrs = 
            fromList [ ( "points", "0.20,0.40 0.70,0.55 0.70,0.25" )
                     , ( "style",        "fill:red")
                     , ("oncontextmenu", "return false;")
                     ] 

    (fEl,_) <- elSvgns "polygon" (constDyn flagAttrs ) $ return ()

    let poleAttrs = 
            fromList [ ( "x1", "0.70" )
                     , ( "y1", "0.25" )
                     , ( "x2", "0.70" )
                     , ( "y2", "0.85" )
                     , ( "stroke-width", ".07")
                     , ( "stroke", "black")
                     , ("oncontextmenu", "return false;")
                     ] 

    (pEl,_) <- elSvgns "line" (constDyn poleAttrs ) $ return ()

    return [fEl, pEl]

showText :: MonadWidget t m => Int -> m [El t]
showText count = do
    elSvgns "text" (constDyn textAttrs) $ text $ pack $ show count
    return []

showCellDetail :: MonadWidget t m => Pos -> Cell -> m [El t]
showCellDetail pos (Cell mined exposed flagged mines) = 
    case (  mined, exposed, flagged, 0 == mines) of
         (      _,       _,    True,     _) -> showFlag pos 
         (   True,    True,       _,     _) -> showMine pos 
         (      _,    True,       _, False) -> showText mines
         (      _,       _,       _,     _) -> return []

mouseEv :: Reflex t => Pos -> El t -> [Event t Msg]
mouseEv pos el = 
    let r_rEv = RightPick pos <$ domEvent Contextmenu el
        l_rEv = LeftPick  pos <$ domEvent Click       el
    in [l_rEv, r_rEv]

showCell :: MonadWidget t m => Pos -> Cell -> m (Event t Msg)
showCell pos cell = 
    fmap snd $ elSvgns "g"  (constDyn $ groupAttrs pos) $ do
        rEl <- showSquare cell
        dEl <- showCellDetail pos cell 
        return $ leftmost $ concatMap (mouseEv pos) (rEl ++ dEl)

showAndReturnCell :: MonadWidget t m => Pos -> Cell -> m (Event t Msg, Cell)
showAndReturnCell pos cell = do
    ev <- showCell pos cell
    return (ev,cell)

adjacents :: Pos -> [Pos]
adjacents (x,y) = 
    [(xx,yy) | xx <- [x-1..x+1]
             , yy <- [y-1..y+1]
             , (xx,yy) /= (x,y)
             , xx >= 0, yy >= 0
             , xx < w, yy < h]

exposeCellList :: [(Pos,Cell)] -> Board -> ([(Pos,Maybe Cell)], Board)
exposeCellList exp board = 
    let exposedMaybe = fmap (\(p,c) -> (p, Just c)) exp
        newBoard = foldl (\b (p,c) -> insert p c b) board exp
    in (exposedMaybe, newBoard)

exposeMines :: State Board [(Pos, Maybe Cell)]
exposeMines = do
    board <- get
    let toExpose = filter (\(pos,cell) -> (not.exposed) cell && mined cell) $ toList board
        exp = fmap (\(p,c) -> (p, c {exposed = True})) toExpose
        (exposedMaybe, newBoard) = exposeCellList exp board
    put newBoard
    return exposedMaybe

exposeSelection :: Pos -> Cell -> Int -> State Board [(Pos, Maybe Cell)]
exposeSelection pos cell count = do
    board <- get
    let cell = board ! pos
        toExpose = if flagged cell then [] else [(pos,cell)]
        exp = fmap (\(p,c) -> (p, c {exposed = True, mines = count})) toExpose
        (exposedMaybe, newBoard) = exposeCellList exp board
    put newBoard
    return exposedMaybe
    

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


fromPick :: Msg -> Board ->[(Pos, Maybe Cell)]
fromPick (LeftPick pos) board = 
    let (nc,_) = runState (exposeCells pos) board
    in nc

fromPick (RightPick pos ) board = 
    let cell = board ! pos
    in if exposed cell
       then [] -- can't flag a cell that's already exposed.
       else [(pos, Just cell {flagged=not $ flagged cell})]

reactToPick :: (Board,Msg) -> Map Pos (Maybe Cell)
reactToPick (b,cell) = fromList $ fromPick cell b

boardAttrs :: Map Text Text
boardAttrs = fromList 
                 [ ("width" , pack $ show $ w * cellSize)
                 , ("height", pack $ show $ h * cellSize)
                 , ("style" , "border:solid; margin:8em")
                 , ("oncontextmenu", "return false;")
                 ]

gameOver :: Board -> Bool
gameOver board = any (\cell -> exposed cell && mined cell) $ (fmap snd . toList) board

showBoard :: MonadWidget t m => m (Dynamic t Bool)
showBoard = do
    gen <- liftIO getStdGen
    let (initial, _)  = runRand mkBoard gen
    rec 
        let pick = switch $ (leftmost . elems) <$> current ev
            pickWithCells = attachPromptlyDynWith (,) cm pick
            updateEv = fmap reactToPick pickWithCells
            eventAndCellMap = listHoldWithKey initial updateEv showAndReturnCell 
            eventMap = fmap (fmap (fmap fst)) eventAndCellMap
        (_, ev) <- elSvgns "svg" (constDyn boardAttrs) eventMap
        let cellMap = fmap (fmap (fmap snd)) eventAndCellMap
        cm <- cellMap 
    return $ fmap gameOver cm

main :: IO ()
main = mainWidget $ do
                        gameOver <- showBoard
                        dyn (fmap (\b -> if b then text "gameover" else text "keepgoing") gameOver )
                        return ()

elSvgns :: MonadWidget t m => Text -> Dynamic t (Map Text Text) -> m a -> m (El t, a)
elSvgns = elDynAttrNS' (Just "http://www.w3.org/2000/svg")
