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

data Cell = Cell { hasBomb :: Bool 
                 , exposed :: Bool
                 , flagged :: Bool
                 }

type Pos = (Int, Int)
type Board = Map Pos Cell

data Cmd = LeftPick Pos Cell | RightPick Pos Cell

width :: Int
width =  40

height :: Int
height =  60

cellSize :: Int
cellSize = 15

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
getColor (Cell hasBomb exposed flagged) = 
    case (hasBomb, exposed, flagged) of
         (      _,       _,    True) -> "red"
         (      _,   False,       _) -> "white"
         (  True ,       _,       _) -> "black"
         (  False,       _,       _) -> "grey"

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

showSquare :: MonadWidget t m => Board -> Pos -> Cell -> m [Event t Cmd]
showSquare board pos c = do
    (rEl,_) <- elSvgns "rect" (constDyn $ cellAttrs c) $ return ()

    let r_rEv = RightPick pos c <$ domEvent Contextmenu rEl
        l_rEv = LeftPick  pos c <$ domEvent Click       rEl

    return [l_rEv, r_rEv]

showText :: MonadWidget t m => Board -> Pos -> Cell -> m [Event t Cmd]
showText board pos c = do
    let count = bombCount board pos

    (tEl,_) <- elSvgns "text" (constDyn textAttrs) $ text $ pack $ show count

    let r_tEv = RightPick pos c <$ domEvent Contextmenu tEl
        l_tEv = LeftPick  pos c <$ domEvent Click       tEl

    return [l_tEv, r_tEv]

showCell :: MonadWidget t m => Board -> Pos -> Cell -> m (Event t Cmd)
showCell board pos c@(Cell _ True _) = do
    (_,ev) <- elSvgns "g" (constDyn $ groupAttrs pos) $ do
                  rEv <- showSquare board pos c
                  tEv <- showText board pos c
                  return $ leftmost $ rEv ++ tEv
    return ev

showCell board pos c@(Cell _ False _) = do
    (_,ev) <- elSvgns "g" (constDyn $ groupAttrs pos) $ do
                  rEv <- showSquare board pos c
                  return $ leftmost rEv 
    return ev

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
    in length $ filter hasBomb $ fmap (board !) indices

fromLeftPickM :: Pos -> State Board [(Pos, Maybe Cell)]
fromLeftPickM (x,y) = 
    state $
        \board ->
            let indices = adjacents (x,y)
                count = length $ filter hasBomb $ fmap (board !) indices
                c = board ! (x,y)

                updatedCell = c {exposed=True} 
                updatedBoard = insert (x,y) updatedCell board 

                checkList = (if not (exposed c) && count == 0 then indices else [] )
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

reactToPick :: Board -> Cmd -> Map Pos (Maybe Cell)
reactToPick b c = fromList $ fromPick b c

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
            updateEv = fmap (reactToPick initialBoard) pick
        (_, ev) <- elSvgns "svg" (constDyn boardAttrs) $ listHoldWithKey initialBoard updateEv (showCell initialBoard)

    return ()

-- At end to avoid Rosetta Code unmatched quotes problem.
elSvgns :: MonadWidget t m => Text -> Dynamic t (Map Text Text) -> m a -> m (El t, a)
elSvgns = elDynAttrNS' (Just "http://www.w3.org/2000/svg")
