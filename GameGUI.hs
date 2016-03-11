-- "THE BEER-WARE LICENSE" (Revision 42):
-- Albatrouss and <skruppy@onmars.eu> wrote this software. As long as you retain
-- this notice you can do whatever you want with this stuff. If we meet some
-- day, and you think this stuff is worth it, you can buy me a beer in return.
--                                                     -- Albatrouss and Skruppy

module GameGUI where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Display.Label
import KI
import Sm
import Data.Array
import Control.Concurrent
import Control.Concurrent.MVar


createGameGUI :: (MVar (Array (Int, Int) String))-> (MVar GameData) -> IO ()
createGameGUI mVField  mVGameData = do 
    initGUI
    field <- takeMVar mVField
    gameData <- takeMVar mVGameData
    let getGameTypeAndName (GameData _ _ gt gn _) = gt ++ " : " ++ gn
    let title = getGameTypeAndName gameData
    window <- windowNew
    set window [windowTitle := title, containerBorderWidth := 5,
                windowDefaultWidth := 500, windowDefaultHeight := 500]
    
    
    let ((_,_),(_,size)) = bounds field
    layoutTable <- tableNew 2 1 False
    -- game field 
    gameTable <- tableNew (size +1) (size +1) True 
    tableAttachDefaults layoutTable gameTable 0 1 0 1
    buttons <- createButtons field gameTable
    addLabels gameTable size
    -- player field
    playerTable <- tableNew 1 2 True
    let getNameFromPlayer (PlayerItem s _ itsMe) = if itsMe then ("  <b>" ++ s  ++ "</b>") else s
    let getPlayername (GameData _ _ _ _ players) n = getNameFromPlayer (players ! n)
    let playername0 = (getPlayername gameData 0) ++ " ⛂"
    let playername1 = (getPlayername gameData 1) ++ " ⛀"
    labelP0 <- labelNew (Just playername0 )
    labelSetMarkup labelP0 playername0
    tableAttachDefaults playerTable labelP0 0 1 0 1
    labelP1 <- labelNew (Just playername1)
    labelSetMarkup labelP1 playername1
    tableAttachDefaults playerTable labelP1 0 1 1 2
    tableAttachDefaults layoutTable playerTable 1 2 0 1
    
    containerAdd window layoutTable
    
    onDestroy window mainQuit
    widgetShowAll window
    
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
                changeLabel (y:ys) (x:xs) = do (buttonSetLabel x (makeCool y)) >> changeLabel ys xs
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