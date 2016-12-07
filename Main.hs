{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


import Reflex
import Reflex.Dom
import Control.Monad.Random (RandomGen, Rand, runRand, getStdGen, getRandomR)
import Control.Monad.Trans (liftIO)
import Control.Monad.State (State, state, runState)
import Data.Map as DM (Map, fromList, elems, lookup, findWithDefault, insert, mapWithKey, (!))
import Data.Text (Text, pack)
import Data.Functor.Misc (dmapToMap, mapWithFunctorToDMap)
import Data.Traversable (forM)

data Cell = Cell { mined :: Bool 
                 , exposed :: Bool
                 , flagged :: Bool
                 } deriving Show

type Pos = (Int, Int)
type Board = Map Pos Cell

data Msg = LeftPick Pos | RightPick Pos 

w :: Int
w =  20

h :: Int
h = 20

cellSize :: Int
cellSize = 20

mkCell :: RandomGen g => Rand g Cell
mkCell = do
    t <- getRandomR (0.0::Float, 1.0)
    return $ Cell (t < 0.201) False False

initBoard :: RandomGen g => [Pos] -> Rand g Board
initBoard positions = do
    cells <- sequence $ repeat mkCell
    return $ (fromList $ zip positions cells)

mkBoard :: RandomGen g => Rand g Board
mkBoard = do
    let positions = [(x,y) | x <- [0..w-1], y <- [0..h-1]]   
    initBoard positions

getColor :: Cell -> String
getColor (Cell _ exposed _) = if exposed then "#909090" else "#AAAAAA"

cellAttrs :: Cell -> Map Text Text
cellAttrs cell = 
    let size = 0.9
        placement = 0.5 - (size/2.0)

    in fromList [ ( "x",            pack $ show placement)
                , ( "y",            pack $ show placement)
                , ( "width",        pack $ show size)
                , ( "height",       pack $ show size)
                , ( "style",        pack $ "fill:" ++ getColor cell)
                , ("oncontextmenu", "return false;")
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

mouseEv :: Reflex t => Pos -> Cell -> El t -> [Event t Msg]
mouseEv pos c el = 
    let r_rEv = RightPick pos <$ domEvent Contextmenu el
        l_rEv = LeftPick  pos <$ domEvent Click       el
    in [l_rEv, r_rEv]


showSquare :: MonadWidget t m => Pos -> Cell -> m [Event t Msg]
showSquare pos c = do
    (rEl,_) <- elSvgns "rect" (constDyn $ cellAttrs c) $ return ()
    return $ mouseEv pos c rEl

showMine :: MonadWidget t m => Pos -> Cell -> m [Event t Msg]
showMine pos c = do
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

    return $ mouseEv pos c cEl ++ mouseEv pos c sEl  

showFlag :: MonadWidget t m => Pos -> Cell -> m [Event t Msg]
showFlag pos c = do
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

    return $ mouseEv pos c fEl ++ mouseEv pos c pEl

showText :: MonadWidget t m => Board -> Pos -> Cell -> m [Event t Msg]
showText board pos c = do
    let count = mineCount board pos
    (tEl,_) <- elSvgns "text" (constDyn textAttrs) $ text $ pack $ show count
    return $ mouseEv pos c tEl

showWithMine :: MonadWidget t m => Board -> Pos -> Cell -> m (Event t Msg, Cell)
showWithMine board pos c = do
                  rEv <- showSquare pos c
                  tEv <- showMine pos c
                  return (leftmost $ rEv ++ tEv,c)

showWithFlag :: MonadWidget t m => Board -> Pos -> Cell -> m (Event t Msg, Cell)
showWithFlag board pos c = do
                  rEv <- showSquare pos c
                  tEv <- showFlag pos c
                  return (leftmost $ rEv ++ tEv,c)

showWithText :: MonadWidget t m => Board -> Pos -> Cell -> m (Event t Msg, Cell)
showWithText board pos c = do
                  showSquare pos c  -- not pickable
                  showText board pos c -- not pickable
                  return (never,c)

showWithoutText :: MonadWidget t m => Board -> Pos -> Cell -> m (Event t Msg, Cell)
showWithoutText board pos c = do
                  rEv <- showSquare pos c
                  return (leftmost rEv ,c)

showInGroup :: MonadWidget t m => Board -> Pos -> Cell -> m (Event t Msg, Cell)
showInGroup board pos c@(Cell mined exposed flagged) = 
                  case (  mined, exposed, flagged, mineCount board pos) of
                       (      _,       _,    True,     _) -> showWithFlag    board pos c
                       (      _,   False,       _,     _) -> showWithoutText board pos c
                       (   True,    True,       _,     _) -> showWithMine    board pos c
                       (      _,    True,       _,     0) -> showWithoutText board pos c
                       (      _,       _,       _,     _) -> showWithText    board pos c



showCell :: MonadWidget t m => Board -> Pos -> Cell -> m (Event t Msg, Cell)
showCell board pos c@(Cell mined _ _) = 
    fmap snd $ elSvgns "g"  (constDyn $ groupAttrs pos) $ showInGroup board pos c 

adjacents :: Pos -> [Pos]
adjacents (x,y) = 
    [(xx,yy) | xx <- [x-1..x+1]
             , yy <- [y-1..y+1]
             , (xx,yy) /= (x,y)
             , xx >= 0, yy >= 0
             , xx < w, yy < h]

mineCount :: Board -> Pos -> Int
mineCount board pos  = 
    length $ filter mined $ fmap (board !) $ adjacents pos

fromLeftPickM :: Pos -> State Board [(Pos, Maybe Cell)]
fromLeftPickM pos = 
    state $
        \board ->
            let indices = adjacents pos
                count = length $ filter mined $ fmap (board !) indices
                c = board ! pos
                
                updatedCell = if flagged c -- can't expose a flagged cell.
                              then c
                              else c {exposed=True} 

                updatedBoard = insert pos updatedCell board 

                checkList = (if exposed c || flagged c || mined c || count /= 0 
                             then [] 
                             else indices 
                             )

                neighborUpdater = mapM fromLeftPickM checkList
                (updatedNeighbors, updatedNeighborsBoard) = runState neighborUpdater updatedBoard
            in ((pos, Just updatedCell) : concat updatedNeighbors, updatedNeighborsBoard)

fromPick :: Board -> Msg -> [(Pos, Maybe Cell)]
fromPick board (LeftPick p) = 
    let (nc,_) = runState (fromLeftPickM p) board
    in nc

fromPick board (RightPick pos ) = 
    let c = board ! pos
    in if exposed c
       then [] -- can't flag a cell that's already exposed.
       else [(pos, Just c {flagged=not $ flagged c})]

reactToPick :: (Board,Msg) -> Map Pos (Maybe Cell)
reactToPick (b,c) = fromList $ fromPick b c

boardAttrs :: Map Text Text
boardAttrs = fromList 
                 [ ("width" , pack $ show $ w * cellSize)
                 , ("height", pack $ show $ h * cellSize)
                 , ("style" , "border:solid; margin:8em")
                 , ("oncontextmenu", "return false;")
                 ]


showCell2 :: forall t m. MonadWidget t m => Dynamic t (Map Pos Cell) -> Pos -> m (Event t Msg)
showCell2 dBoard pos = do
    let dCell = fmap (findWithDefault (Cell False False False) pos) dBoard
    (el, _) <- elSvgns "g"  (constDyn $ groupAttrs pos) $ 
                   elSvgns "rect" (fmap cellAttrs dCell) $ 
                       return ()
    return $ LeftPick pos <$ domEvent Click el 


updateBoard (LeftPick pos) oldBoard = insert pos (Cell False False True) oldBoard

showBoard2 :: forall t m. MonadWidget t m => m ()
showBoard2 = do 
    gen <- liftIO getStdGen
    rec 
        let 
            indices = [(x,y) | x <- [0..w-1], y <- [0..h-1]] 
            (initialBoard, _)  = runRand (initBoard indices) gen
        board <- foldDyn updateBoard initialBoard pickEv
        (el, ev) <- elSvgns "svg" (constDyn boardAttrs) $ forM indices $ showCell2 board
        let pickEv =  leftmost ev
    return ()

showBoard :: MonadWidget t m => m ()
showBoard = do
    gen <- liftIO getStdGen
    let (initial, _)  = runRand mkBoard gen
    rec 
        let pick = switch $ (leftmost . elems) <$> current ev
            pickWithCells = attachPromptlyDynWith (,) cm pick
            updateEv = fmap reactToPick pickWithCells
            eventAndCellMap = listHoldWithKey initial updateEv (showCell initial)
            eventMap = fmap (fmap (fmap fst)) eventAndCellMap
        (_, ev) <- elSvgns "svg" (constDyn boardAttrs) eventMap
        let cellMap = fmap (fmap (fmap snd)) eventAndCellMap
        cm <- cellMap 
    return ()

main :: IO ()
main = mainWidget showBoard2

-- At end to avoid Rosetta Code unmatched quotes problem.
elSvgns :: MonadWidget t m => Text -> Dynamic t (Map Text Text) -> m a -> m (El t, a)
elSvgns = elDynAttrNS' (Just "http://www.w3.org/2000/svg")


