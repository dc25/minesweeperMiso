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

data Cmd = Pick (Int, Int) Cell

width :: Int
width =  40

height :: Int
height =  30

cellSize :: Int
cellSize = 20

update :: Cmd -> Board -> Board
update (Pick (x,y) c) b = b

mkBoard :: RandomGen g => Rand g Board
mkBoard = do
    let cells = [((x,y),Cell 1) | x <- [0..width-1], y <- [0..height-1]]
    return $ Board $ fromList cells

cellToAttrs :: (Int, Int) -> Map Text Text
cellToAttrs (x,y) = do
    let size = 0.9
        placement = 0.5 - size / 2.0

    fromList [ ( "x", pack $ show $ fromIntegral x+placement)
             , ( "y", pack $ show $ fromIntegral y+placement)
             , ( "width",  pack $ show $ size)
             , ( "height",  pack $ show $ size)
             ] 

showCell :: MonadWidget t m => (Int, Int) -> Cell -> m (Event t Cmd)
showCell pos c = do
    let dCellAttrs = constDyn (cellToAttrs pos) 

    (el,_) <- elStopPropagationNS svgns "g" Mousedown $ 
                 elDynAttrNS' svgns "rect" dCellAttrs $ return ()

    let domEv = domEvent Mousedown el
        pickEv = fmap (\_ -> Pick pos c) domEv

    return pickEv

main :: IO ()
main = mainWidget $ do
    gen <- liftIO getStdGen
    let (initialBoard, _)  = runRand mkBoard gen
        attrs = constDyn $ 
                    fromList 
                        [ ("width" , pack $ show (width*cellSize))
                        , ("height", pack $ show (height*cellSize))
                        , ("viewBox", pack ("0 0 " ++ show width ++ " " ++ show height))
                        , ("style" , "border:solid; margin:8em")
                        ]

        cellMap = board initialBoard
    rec 
        let pick = switch $ (leftmost . elems) <$> current ev
            removal = \(Pick pos c) -> (pos =: Nothing)
            removeEv = fmap removal pick
        (_, ev) <- elDynAttrNS' svgns "svg" attrs $ listHoldWithKey cellMap removeEv showCell

    return ()

svgns :: Maybe Text
svgns = (Just "http://www.w3.org/2000/svg")
