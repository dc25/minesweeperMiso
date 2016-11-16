{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}

import Reflex
import Reflex.Dom
import Control.Monad.Random (RandomGen, Rand, runRand, getStdGen)
import Control.Monad.Trans (liftIO)
import Data.Map (Map, fromList, elems)
import Data.Text (Text, pack)

data Cell = Cell Int

data Board = Board { board :: (Map (Int,Int) Cell) }

data Cmd = Pick (Int, Int)

width :: Int
width =  8

height :: Int
height =  6

cellSize :: Int
cellSize = 50

update :: Cmd -> Board -> Board
update (Pick (x,y)) b = b

mkBoard :: RandomGen g => Rand g Board
mkBoard = do
    let cells = [((x,y),Cell 1) | x <- [0..width-1], y <- [0..height-1]]
    return $ Board $ fromList cells

cellToAttrs :: (Int, Int) -> Cell -> Map Text Text
cellToAttrs (x,y) cell = do
    let size = 0.9
        placement = 0.5 - size / 2.0

    fromList [ ( "x", pack $ show $ fromIntegral x+placement)
             , ( "y", pack $ show $ fromIntegral y+placement)
             , ( "width",  pack $ show $ size)
             , ( "height",  pack $ show $ size)
             ] 

showCell :: MonadWidget t m => (Int, Int) -> Dynamic t Cell -> m (Event t Cmd)
showCell pos dCell = do
    let dCellAttrs = fmap (cellToAttrs pos) dCell

    (el,_) <- elStopPropagationNS svgns "g" Mousedown $ 
                 elDynAttrNS' svgns "rect" dCellAttrs $ return ()

    return $ fmap (const $ Pick (0,0)) $ domEvent Mousedown el
    

view :: MonadWidget t m => Dynamic t Board -> m (Event t Cmd)
view dboard = do
    let attrs = constDyn $ 
                    fromList 
                        [ ("width" , pack $ show (width*cellSize))
                        , ("height", pack $ show (height*cellSize))
                        , ("viewBox", pack ("0 0 " ++ show width ++ " " ++ show height))
                        , ("style" , "border:solid; margin:8em")
                        ]

        cellMap = fmap board dboard

    (_, dPopEventMap) <- elDynAttrNS' svgns "svg" attrs $ listWithKey cellMap showCell

    return $ switch $ (leftmost . elems) <$> current dPopEventMap

main :: IO ()
main = mainWidget $ do
    gen <- liftIO getStdGen
    let (initialBoard, _)  = runRand mkBoard gen
    rec 
        board <- foldDyn update initialBoard =<< view board
    return ()

svgns :: Maybe Text
svgns = (Just "http://www.w3.org/2000/svg")
