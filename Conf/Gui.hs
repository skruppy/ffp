-- "THE BEER-WARE LICENSE" (Revision 42):
-- Albatrouss and <skruppy@onmars.eu> wrote this software. As long as you retain
-- this notice you can do whatever you want with this stuff. If we meet some
-- day, and you think this stuff is worth it, you can buy me a beer in return.
--                                                     -- Albatrouss and Skruppy

module Conf.Gui where

import Graphics.UI.Gtk
import Util
import Conf
import Data.Maybe
import System.Exit
import Data.String.Utils


data Gui = Gui
    { guiWindow   :: Dialog
    , guiInHost   :: Entry
    , guiInPort   :: Entry
    , guiInGameId :: Entry
    , guiInPlayer :: Entry
    , guiLabel    :: Label
    }

labeledInputNew table row name text = do
    input <- entryNew
    entrySetText input text
    tableAttachDefaults table input  1 2  (row) (row+1)
    
    label <- labelNewWithMnemonic name
    miscSetAlignment label 1.0 0.5
    labelSetMnemonicWidget label input
    tableAttach table label  0 1  (row) (row+1) [Fill] [Fill] 0 0
    
    return input


foo input = if length text > 0 then Just text else Nothing where text = strip input


createGui defaults = do
    initGUI
    
    window <- dialogNew
    set window [windowTitle := "Funthello", containerBorderWidth := 10]
    
    dialogContainer <- dialogGetUpper window
    grid <- tableNew 0 0 False
    tableSetRowSpacings grid 4
    tableSetColSpacings grid 6
    boxPackStart dialogContainer grid PackNatural 0
    
    btCancel  <- dialogAddButton window stockCancel ResponseCancel
    btConnect <- dialogAddButton window stockConnect ResponseOk
    
    inGameId <- labeledInputNew grid 0 "Game _ID"   (fromMaybe "" $ gameId defaults)
    inPlayer <- labeledInputNew grid 1 "P_layer ID" (maybe "" show $ player defaults)
    inHost   <- labeledInputNew grid 2 "Ho_stname"  (fromMaybe "" $ host defaults)
    inPort   <- labeledInputNew grid 3 "_Port"      (fromMaybe "" $ port defaults)
    
    label <- labelNew Nothing
    labelSetJustify label JustifyLeft
    labelSetLineWrap label True
    miscSetAlignment label 0.0 0.5
    boxPackStart dialogContainer label PackGrow 0
    
    widgetShowAll window
    dialogSetDefaultResponse window ResponseOk
    
    return Gui
        { guiWindow   = window
        , guiInHost   = inHost
        , guiInPort   = inPort
        , guiInGameId = inGameId
        , guiInPlayer = inPlayer
        , guiLabel    = label
        }


-- https://rosettacode.org/wiki/User_input/Graphical#Haskell
-- https://searchcode.com/codesearch/view/21757632/
getCfg gui msg = do
    labelSetMarkup (guiLabel gui) ("<span color=\"red\" weight=\"bold\">"++msg++"</span>")
    dialogResult <- dialogRun $ guiWindow gui
    
    dummy <- case dialogResult of
        ResponseCancel      -> exitSuccess -- User clicked "cancel"
        ResponseDeleteEvent -> exitSuccess -- User closed the window
        _ -> return ()
    
--     widgetHide window
    
    txtHost   <- entryGetText $ guiInHost gui
    txtPort   <- entryGetText $ guiInPort gui
    txtGameId <- entryGetText $ guiInGameId gui
    txtPlayer <- entryGetText $ guiInPlayer gui
    return IntermediateCfg
        { host   = foo txtHost
        , port   = foo txtPort
        , conf   = Nothing
        , gameId = foo txtGameId
        , player = (let text = strip txtPlayer in if length text > 0 then Just $ read text else Nothing)
        }
