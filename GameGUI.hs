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


createGameGUI :: (MVar (Array (Int, Int) String))-> (MVar GameData) -> IO ()
createGameGUI mVField  mVGameData = do 
    initGUI
    window <- windowNew
    set window [windowTitle := "GUI", containerBorderWidth := 5,
                windowDefaultWidth := 500, windowDefaultHeight := 500]
                
    field <- takeMVar mVField
    gameData <- takeMVar mVGameData
    let ((_,_),(_,size)) = bounds field
    table <- tableNew (size +1) (size +1) True 
    
    buttons <- createButtons field table
    addLabels table size
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
    let r' = r +1
    let c' = size - c +1
    button <- buttonNewWithLabel s
    tableAttachDefaults table button  (r'-1) (r') (c'-1) (c')
    return button
    
addLabels :: Table -> Int -> IO ()
addLabels table size = do
    let toList 0 = []
        toList x = x : (toList (x-1))
    let nums = toList size
    let labelsNum = map (\y -> show y) nums
    let labelsAlpha = map (\x -> (alphabet !! (x-1):"")) nums
    let numIndi = map (\x -> (size - x + 1,1)) nums
    let alphaIndi = map (\x -> (size+1, x +1)) nums
    nlabels <- createLabels table numIndi labelsNum
    alabels <- createLabels table alphaIndi labelsAlpha
    return ()
    
    
    
    
createLabels :: Table -> [(Int,Int)] -> [String] -> IO ([Label])
createLabels _     []         _           = do return []
createLabels table coord labelStrings = do
    labels <- mapM labelNew (map Just labelStrings)
    let trippel = zip labels coord
    mapM_ (\(l,(c,r)) -> tableAttachDefaults table l (r-1) r (c-1) c) trippel
    return labels
    
    
    
    
    
    
    