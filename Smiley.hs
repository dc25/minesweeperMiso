{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Smiley ( showFace) where

import Reflex
import Reflex.Dom
import Data.Map (fromList)
import Data.Text (pack)
import Data.Traversable

import Svg

showFace :: MonadWidget t m => Bool -> m ()
showFace lost = do
        let sz = 100::Int
        elSvgns "svg" (constDyn $ fromList [ ("width", "100")
                                           , ("height", "100")  
                                           ]) $ 
            elSvgns "g" (constDyn $ fromList [ ("transform", pack $     "scale (" ++ show sz ++ ", " ++ show sz ++ ") " ++ "translate (0.5, 0.5)"  ) ]) $ do

                -- face outline
                elSvgns "circle" (constDyn $ 
                                 fromList [ ( "cx", "0.0" )
                                          , ( "cy", "0.0" )
                                          , ( "r",  "0.4" ) 
                                          , ("style", "fill:yellow") 
                                          , ("stroke", "black") 
                                          , ("stroke-width", "0.02") 
                                          ]
                                 ) $ return ()
                -- right eye
                elSvgns "circle" (constDyn $ 
                                 fromList [ ( "cx", "0.15" )
                                          , ( "cy", "-0.1" )
                                          , ( "r",  "0.08" ) 
                                          , ("style", "fill:yellow") 
                                          , ("stroke", "black") 
                                          , ("stroke-width", "0.02") 
                                          ]
                                 ) $ return ()

                -- left eye
                elSvgns "circle" (constDyn $ 
                                 fromList [ ( "cx", "-0.15" )
                                          , ( "cy", "-0.1" )
                                          , ( "r",  "0.08" ) 
                                          , ("style", "fill:yellow") 
                                          , ("stroke", "black") 
                                          , ("stroke-width", "0.02") 
                                          ]
                                 ) $ return ()

                if lost then do
                    -- eye crosses
                    fmap head 
                        (forM [ (ex, dx, dy)::(Float, Float, Float) 
                                            | ex <- [-0.15, 0.15],
                                              dx <- [-0.1, 0.1],
                                              dy <- [-0.1, 0.1] ]
                            ( \(ex, px,py) -> elSvgns "path" (constDyn $ 
                                             fromList [ ("d", pack $ "M " ++ show ex ++ " -0.1 l " ++ show px ++ " " ++ show py)
                                                      , ("stroke", "black") 
                                                      , ("stroke-width", "0.02") 
                                                      , ("fill", "none") 
                                                      ]
                                             ) $ return () ) )

                else do
                    -- right eyeball
                    elSvgns "circle" (constDyn $ 
                                     fromList [ ( "cx", "0.15" )
                                              , ( "cy", "-0.1" )
                                              , ( "r",  "0.04" ) 
                                              , ("style", "fill:black") 
                                              ]
                                     ) $ return ()
                    -- left eyeball
                    elSvgns "circle" (constDyn $ 
                                     fromList [ ( "cx", "-0.15" )
                                              , ( "cy", "-0.1" )
                                              , ( "r",  "0.04" ) 
                                              , ("style", "fill:black") 
                                              ]
                                     ) $ return ()
                -- smile/frown
                elSvgns "path" (constDyn $ 
                                 fromList [ ("d", pack $ "M-0.15,0.15 a0.2,0.2 0 0 " ++ (if lost then "1" else "0") ++ " 0.30,0.0")
                                          , ("stroke", "black") 
                                          , ("stroke-width", "0.02") 
                                          , ("fill", "none") 
                                          ]
                                 ) $ return ()

        return ()
