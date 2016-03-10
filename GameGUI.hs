module Game.GUI where

import Graphics.UI.Gtk
import KI
import Sm
import Data.Array
import Control.Concurrent
import Control.Concurrent.MVar


createGameGUI :: (MVar (Array (Int, Int) String)) -> [PlayerItem] -> IO ()
createGameGUI mVField players = do 
    initGUI
    window <- windowNew
    set window [windowTitle := "GUI", containerBorderWidth := 5,
                windowDefaultWidth := 500, windowDefaultHeight := 500]
                
    field <- takeMVar mVField 
    let ((_,_),(_,size)) = bounds field
    table <- tableNew size size True 
    buttons <- createButtons field table
    
    containerAdd window table
    onDestroy window mainQuit
    widgetShowAll window
    --windowPresent window
    timeoutAdd (updateField mVField buttons >> return True) 100 
    
    mainGUI
                        
                        
                        
updateField :: (MVar (Array (Int, Int) String)) -> [Button] -> IO()
updateField mVField buttons = do
    newField <- takeMVar mVField
    let i = indices newField
    let e = elems newField
    let changeLabel (y:[]) (x:[]) = do (buttonSetLabel x y)
        changeLabel (y:ys) (x:xs)= do (buttonSetLabel x y) >> changeLabel ys xs
    changeLabel e buttons
    
                            
createButtons :: (Array (Int, Int) String)-> Table -> IO [Button]
createButtons field table = do 
    let fieldIndi = indices field
    buttons <- mapM (createButton table field) fieldIndi
    return buttons
                              
createButton :: Table -> (Array (Int, Int) String) -> (Int,Int) -> IO Button
createButton table field (r,c) = do 
    let s = field ! (r,c)
    button <- buttonNewWithLabel s
    tableAttachDefaults table button (c-1) (c) (r-1) (r)
    return button
    
