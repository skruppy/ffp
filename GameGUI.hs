-- "THE BEER-WARE LICENSE" (Revision 42):
-- Albatrouss and <skruppy@onmars.eu> wrote this software. As long as you retain
-- this notice you can do whatever you want with this stuff. If we meet some
-- day, and you think this stuff is worth it, you can buy me a beer in return.
--                                                     -- Albatrouss and Skruppy

module GameGUI where

import Graphics.UI.Gtk
import KI
import Sm
import Data.Array
import Control.Concurrent
import Control.Concurrent.MVar


createGameGUI :: (MVar (Array (Int, Int) String)) -> IO ()
createGameGUI mVField  = do 
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
    timeoutAdd ((updateField mVField buttons)>> yield >> return True) 100
    
    mainGUI
                        
                        
updateField :: (MVar (Array (Int, Int) String)) -> [Button] -> IO()
updateField mVField buttons = do
    maybeNewField <- tryTakeMVar mVField
    case maybeNewField of
         Nothing ->  do return ()
         Just newField -> do
            let i = indices newField
            let e = elems newField
            let makeCool a = if (a == "W") then "⛀" else if (a == "B") then "⛂" else " " 
            let changeLabel (y:[]) (x:[]) = do (buttonSetLabel x (makeCool y))
                changeLabel (y:ys) (x:xs)= do (buttonSetLabel x (makeCool y)) >> changeLabel ys xs
            changeLabel e buttons
    
                            
createButtons :: (Array (Int, Int) String)-> Table -> IO [Button]
createButtons field table = do 
    let fieldIndi = indices field
    buttons <- mapM (createButton table field) fieldIndi
    return buttons
                              
createButton :: Table -> (Array (Int, Int) String) -> (Int,Int) -> IO Button
createButton table field (r,c) = do 
    let s = if ((field ! (r,c)) == "W") then "⛀" else if ((field ! (r,c)) == "B") then "⛂" else " " 
    let ((_,_),(_, size)) = bounds field
    let r' = r
    let c' = size - c +1
    button <- buttonNewWithLabel s
    tableAttachDefaults table button  (r'-1) (r') (c'-1) (c')
    return button
    
