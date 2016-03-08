module Conf.Gui (getCfg) where

import Graphics.UI.Gtk
import Util
import Conf
import Data.Maybe
import System.Exit
import Data.String.Utils


labeledInputNew table row name text = do
    label <- labelNew (Just name)
    labelSetJustify label JustifyRight
    tableAttachDefaults table label  0 1  (row) (row+1)

    input <- entryNew
    entrySetText input text
    tableAttachDefaults table input  1 2  (row) (row+1)
    return input


foo input = if length text > 0 then Just text else Nothing where text = strip input


-- https://rosettacode.org/wiki/User_input/Graphical#Haskell
-- https://searchcode.com/codesearch/view/21757632/
getCfg defaults = do
    initGUI
    
    window <- dialogNew
    set window [windowTitle := "Sysprak client", containerBorderWidth := 10]
    
    dialogContainer <- dialogGetUpper window
    grid <- tableNew 0 0 False
    boxPackStart dialogContainer grid PackNatural 0
    
    btCancel  <- dialogAddButton window stockCancel ResponseCancel
    btConnect <- dialogAddButton window stockConnect ResponseOk
    
    inGameId <- labeledInputNew grid 0 "Game ID"   (fromMaybe "" $ gameId defaults)
    inPlayer <- labeledInputNew grid 1 "Player ID" (maybe "" show $ player defaults)
    inHost   <- labeledInputNew grid 2 "Hostname"  (fromMaybe "" $ host defaults)
    inPort   <- labeledInputNew grid 3 "Port"      (fromMaybe "" $ port defaults)
    
    widgetShowAll window
    dialogResult <- dialogRun window
    
    dummy <- case dialogResult of
        ResponseCancel -> exitSuccess
        _ -> return ()
    
    txtHost   <- entryGetText inHost
    txtPort   <- entryGetText inPort
    txtGameId <- entryGetText inGameId
    txtPlayer <- entryGetText inPlayer
    return IntermediateCfg
        { host   = foo txtHost
        , port   = foo txtPort
        , conf   = Nothing
        , gameId = foo txtGameId
        , player = (let text = strip txtPlayer in if length text > 0 then Just $ read text else Nothing)
        }
