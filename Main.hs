{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}

import Reflex
import Reflex.Dom
import Control.Monad.Random (RandomGen, Rand, runRand, getStdGen, getRandomR)
import Control.Monad.Trans (liftIO)
import Data.Map (Map, fromList, elems)
import Data.Text (Text, pack)

svgns :: Maybe Text
svgns = Just "http://www.w3.org/2000/svg"

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
height =  30

cellSize :: Int
cellSize = 24

update :: Cmd -> Board -> Board
update (LeftPick (x,y) c) b = b

mkCell :: RandomGen g => Rand g Cell
mkCell = do
    t <- getRandomR (0.0::Float, 1.0::Float)
    return $ Cell (t < (0.2::Float)) False False

mkBoard :: RandomGen g => Rand g Board
mkBoard = do
    cells <- sequence $ repeat mkCell
    return $ 
        fromList $ 
        zip [(x,y) | x <- [0..width-1], y <- [0..height-1]] cells 

getColor :: Cell -> String
getColor (Cell hasBomb exposed flagged) = 
    case (hasBomb, exposed, flagged) of
        (_,_,True) -> "red"
        (_,False,_) -> "green"
        (True,_,_) -> "black"
        (False,_,_) -> "grey"

cellToAttrs :: (Int, Int) -> Cell -> Map Text Text
cellToAttrs (x,y) cell = do
    let size = 0.9
        placement = 0.5 - size / 2.0

    fromList [ ( "x", pack $ show $ fromIntegral x+placement)
             , ( "y", pack $ show $ fromIntegral y+placement)
             , ( "width",  pack $ show size)
             , ( "height",  pack $ show size)
             , ( "style",  pack $ "fill:" ++ getColor cell)
             , ("oncontextmenu", "return false;")
             ] 

showCell :: MonadWidget t m => (Int, Int) -> Cell -> m (Event t Cmd)
showCell pos c = do
    let dCellAttrs = constDyn (cellToAttrs pos c) 
    (el,_) <- elDynAttrNS' svgns "rect" dCellAttrs $ return ()
    let lEv = const (RightPick pos c) <$> domEvent Contextmenu el
        rEv = const (LeftPick pos c) <$> domEvent Click el
    return $ leftmost [lEv, rEv]

reactToPick :: Cmd -> Map (Int, Int) (Maybe Cell)
reactToPick (LeftPick pos c) = pos =: Just c {exposed=True} 
reactToPick (RightPick pos c) = pos =: Just c {flagged=not $ flagged c} 

boardAttrs = fromList 
                 [ ("width" , pack $ show (width*cellSize))
                 , ("height", pack $ show (height*cellSize))
                 , ("viewBox", pack ("0 0 " ++ show width ++ " " ++ show height))
                 , ("style" , "border:solid; margin:8em")
                 , ("oncontextmenu", "return false;")
                 ]
main :: IO ()
main = mainWidget $ do
    gen <- liftIO getStdGen
    let (initialBoard, _)  = runRand mkBoard gen
    rec 
        let pick = switch $ (leftmost . elems) <$> current ev
            updates = fmap reactToPick pick
        (_, ev) <- elDynAttrNS' svgns "svg" (constDyn boardAttrs) $ listHoldWithKey initialBoard updates showCell

    return ()

