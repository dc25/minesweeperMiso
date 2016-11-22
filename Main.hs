{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
import Reflex
import Reflex.Dom
import Control.Monad.Random (RandomGen, Rand, runRand, getStdGen, getRandomR)
import Control.Monad.Trans (liftIO)
import Control.Monad.State (State, state, runState)
import Data.Map as DM (Map, fromList, elems, lookup, insert, (!))
import Data.Maybe (catMaybes)
import Data.Text (Text, pack)

data Cell = Cell { mined :: Bool 
                 , exposed :: Bool
                 , flagged :: Bool
                 }

type Pos = (Int, Int)
type Board = Map Pos Cell

data Cmd = LeftPick Pos Cell | RightPick Pos Cell

width :: Int
width =  32

height :: Int
height =  16

cellSize :: Int
cellSize = 20

mkCell :: RandomGen g => Rand g Cell
mkCell = do
    t <- getRandomR (0.0::Float, 1.0)
    return $ Cell (t < 0.2) False False

mkBoard :: RandomGen g => Rand g Board
mkBoard = do
    let positions = [(x,y) | x <- [0..width-1], y <- [0..height-1]]
    cells <- sequence $ repeat mkCell
    return $ fromList $ zip positions cells 

getColor :: Cell -> String
getColor (Cell mined exposed flagged) = 
    case (mined, exposed, flagged) of
         (      _,       _,    True) -> "#AAAAAA"
         (      _,   False,       _) -> "#AAAAAA"
         (  True ,       _,       _) -> "black"
         (  False,       _,       _) -> "#909090"

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
    fromList [ ( "x",            "0.5")
             , ( "y",            "0.6")
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

mouseEv :: Reflex t => Pos -> Cell -> El t -> [Event t Cmd]
mouseEv pos c el = 
    let r_rEv = RightPick pos c <$ domEvent Contextmenu el
        l_rEv = LeftPick  pos c <$ domEvent Click       el
    in [l_rEv, r_rEv]


showSquare :: MonadWidget t m => Pos -> Cell -> m [Event t Cmd]
showSquare pos c = do
    (rEl,_) <- elSvgns "rect" (constDyn $ cellAttrs c) $ return ()
    return $ mouseEv pos c rEl

showFlag :: MonadWidget t m => Pos -> Cell -> m [Event t Cmd]
showFlag pos c = do
    let flagAttrs = 
            fromList [ ( "points", "0.20,0.40 0.70,0.55 0.70,0.25" )
                     , ( "style",        "fill:red")
                     , ("oncontextmenu", "return false;")
                     ] 

    (fEl,_) <- elSvgns "polygon" (constDyn $ flagAttrs ) $ return ()

    let poleAttrs = 
            fromList [ ( "x1", "0.70" )
                     , ( "y1", "0.25" )
                     , ( "x2", "0.70" )
                     , ( "y2", "0.85" )
                     , ( "stroke-width", ".07")
                     , ( "stroke", "black")
                     , ("oncontextmenu", "return false;")
                     ] 

    (pEl,_) <- elSvgns "line" (constDyn $ poleAttrs ) $ return ()

    return $ mouseEv pos c fEl ++ mouseEv pos c pEl

showText :: MonadWidget t m => Board -> Pos -> Cell -> m [Event t Cmd]
showText board pos c = do
    let count = bombCount board pos
    (tEl,_) <- elSvgns "text" (constDyn textAttrs) $ text $ pack $ show count
    return $ mouseEv pos c tEl

showWithFlag :: MonadWidget t m => Board -> Pos -> Cell -> m ((Event t Cmd), Cell)
showWithFlag board pos c = do
    (_,ev) <- elSvgns "g" (constDyn $ groupAttrs pos) $ do
                  rEv <- showSquare pos c
                  tEv <- showFlag pos c
                  return $ leftmost $ rEv ++ tEv
    return (ev,c)

showWithText :: MonadWidget t m => Board -> Pos -> Cell -> m ((Event t Cmd), Cell)
showWithText board pos c = do
    (_,ev) <- elSvgns "g" (constDyn $ groupAttrs pos) $ do
                  showSquare pos c  -- not pickable
                  showText board pos c -- not pickable
                  return never
    return (ev,c)

showWithoutText :: MonadWidget t m => Board -> Pos -> Cell -> m ((Event t Cmd), Cell)
showWithoutText board pos c = do
    (_,ev) <- elSvgns "g" (constDyn $ groupAttrs pos) $ do
                  rEv <- showSquare pos c
                  return $ leftmost rEv 
    return (ev,c)

showCell :: MonadWidget t m => Board -> Pos -> Cell -> m ((Event t Cmd), Cell)
showCell board pos c@(Cell mined exposed flagged) = 
    let count = bombCount board pos
    in case (  mined, exposed, flagged, count) of
            (      _,       _,    True,     _) -> showWithFlag    board pos c
            (      _,   False,       _,     _) -> showWithoutText board pos c
            (      _,    True,       _,     0) -> showWithoutText board pos c
            (      _,       _,       _,     _) -> showWithText    board pos c

adjacents :: Pos -> [Pos]
adjacents (x,y) = 
    [(xx,yy) | xx <- [x-1..x+1]
             , yy <- [y-1..y+1]
             , (xx,yy) /= (x,y)
             , xx >= 0, yy >= 0
             , xx < width, yy < height]

bombCount :: Board -> Pos -> Int
bombCount board (x,y)  = 
    let indices = adjacents (x,y)
    in length $ filter mined $ fmap (board !) indices

fromLeftPickM :: Pos -> State Board [(Pos, Maybe Cell)]
fromLeftPickM (x,y) = 
    state $
        \board ->
            let indices = adjacents (x,y)
                count = length $ filter mined $ fmap (board !) indices
                c = board ! (x,y)
                
                updatedCell = if (not $ flagged c) -- can't expose a flagged cell.
                              then c {exposed=True} 
                              else c

                updatedBoard = insert (x,y) updatedCell board 

                checkList = (if     not (exposed c) 
                                 && not (flagged c) 
                                 && count == 0 
                             then indices 
                             else [] )

                neighborUpdater = mapM fromLeftPickM checkList
                (updatedNeighbors, updatedNeighborsBoard) = runState neighborUpdater updatedBoard
            in (((x,y), Just updatedCell) : concat updatedNeighbors, updatedNeighborsBoard)

fromPick :: Board -> Cmd -> [(Pos, Maybe Cell)]
fromPick board (LeftPick p c) = 
    let (nc,_) = runState (fromLeftPickM p) board
    in nc

fromPick board (RightPick pos c) = 
    if exposed c 
    then [] -- can't flag a cell that's already exposed.
    else [(pos, Just c {flagged=not $ flagged c})]

reactToPick :: (Board,Cmd) -> Map Pos (Maybe Cell)
reactToPick (b,c) = fromList $ fromPick b c

boardAttrs = fromList 
                 [ ("width" , pack $ show $ width * cellSize)
                 , ("height", pack $ show $ height * cellSize)
                 , ("style" , "border:solid; margin:8em")
                 , ("oncontextmenu", "return false;")
                 ]
main :: IO ()
main = mainWidget $ do
    gen <- liftIO getStdGen
    let (initialBoard, _)  = runRand mkBoard gen
    rec 
        let pick = switch $ (leftmost . elems) <$> current ev
            pickWithCells = attachPromptlyDynWith (,) cm pick
            updateEv = fmap reactToPick pickWithCells
            eventAndCellMap = listHoldWithKey initialBoard updateEv (showCell initialBoard)
            cellMap = fmap (fmap (fmap snd)) eventAndCellMap
            eventMap = fmap (fmap (fmap fst)) eventAndCellMap
        cm <- cellMap 
        (_, ev) <- elSvgns "svg" (constDyn boardAttrs) $ eventMap
    return ()

-- At end to avoid Rosetta Code unmatched quotes problem.
elSvgns :: MonadWidget t m => Text -> Dynamic t (Map Text Text) -> m a -> m (El t, a)
elSvgns = elDynAttrNS' (Just "http://www.w3.org/2000/svg")
