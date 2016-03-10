module Game.GUI where

import Graphics.UI.Gtk
import KI
import Sm
import Data.Array


createGameGUI :: (Array (Int, Int) String) -> [PlayerItem] -> IO ()
createGameGUI field players = do 
                                initGUI
                                window <- windowNew
                                set window [windowTitle := "GUI", containerBorderWidth := 5,
                                            windowDefaultWidth := 200, windowDefaultHeight := 200]
              
                                let ((_,_),(sizeR,sizeC)) = bounds field
                                --let (startPlayer, endPlayer) = bounds players
                                let fieldElems = elems field
                                let fieldIndi = indices field
                                --let playerElems = elems players
                                
                                table <- tableNew sizeR sizeC True
                                
                                labels <- mapM (createLabel table field) fieldIndi
                                
                                containerAdd window table
                                onDestroy window mainQuit
                                widgetShowAll window
                                mainGUI
                                
                                
                                
                                
                                
createLabel :: Table -> (Array (Int, Int) String) -> (Int,Int) -> IO Label
createLabel table field (r,c) = do 
    let s = field ! (r,c)
    label <- labelNew (Just s)
    tableAttachDefaults table label (c-1) (c) (r-1) (r)
    return label
    
    