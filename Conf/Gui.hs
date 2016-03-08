module Conf.Gui (getCfg) where

import Graphics.UI.Gtk
import Util
import Conf


labeledInputNew table row name = do
    label <- labelNew (Just name)
    labelSetJustify label JustifyRight
    tableAttachDefaults table label  0 1  (row) (row+1)

    input <- entryNew
    tableAttachDefaults table input  1 2  (row) (row+1)
    return input


foo input = if length text > 0 then Just text else Nothing where text = trim input


-- https://rosettacode.org/wiki/User_input/Graphical#Haskell
-- https://searchcode.com/codesearch/view/21757632/
getCfg = do
    initGUI

    window <- dialogNew
    set window [windowTitle := "Sysprak client", containerBorderWidth := 10]

    vb' <- dialogGetUpper window
    vb <- tableNew 0 0 False
    boxPackStart vb' vb PackNatural 0

    dialogAddButton window stockYes ResponseYes
    dialogAddButton window stockNo ResponseNo

    -- First line
    inGameId <- labeledInputNew vb 0 "Game ID"
    inPlayer <- labeledInputNew vb 1 "Player ID"
    inServer <- labeledInputNew vb 2 "Hostname"
    inPort   <- labeledInputNew vb 3 "Port"

    widgetShowAll window

    onDestroy window mainQuit
    mainGUI
    a <- entryGetText inServer
    b <- entryGetText inPort
    c <- entryGetText inGameId
    d <- entryGetText inPlayer
    
    return IntermediateCfg
        { host   = foo a
        , port   = foo b
        , conf   = Nothing
        , gameId = foo c
        , player = (let text = trim d in if length text > 0 then Just $ read text else Nothing)
        }