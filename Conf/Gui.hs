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
import Control.Concurrent.MVar
import Control.Concurrent


labeledInputNew table row name text = do
    input <- entryNew
    entrySetText input text
    tableAttachDefaults table input  1 2  (row) (row+1)
    
    label <- labelNewWithMnemonic name
    miscSetAlignment label 1.0 0.5
    labelSetMnemonicWidget label input
    tableAttach table label  0 1  (row) (row+1) [Fill] [Fill] 0 0
    
    return input


bar input =
    if length text > 0
        then Just text
        else Nothing
    where text = strip input


getCfg defaults connect = do
    result <- newMVar Nothing
    
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
    
    label <- labelNew $ Just ""
    labelSetJustify label JustifyLeft
    labelSetLineWrap label True
    miscSetAlignment label 0.0 0.5
    boxPackStart dialogContainer label PackGrow 0
    
    spinner <- spinnerNew
    boxPackStart dialogContainer spinner PackGrow 0
    
    widgetShowAll window
    dialogSetDefaultResponse window ResponseOk
    
    window `on` response $ \res -> do
        case res of
            -- User clicked "cancel"
            ResponseCancel      -> mainQuit
            
            -- User closed the window
            ResponseDeleteEvent -> mainQuit
            
            -- User clicked "Connect"
            _                   -> do
                txtHost   <- entryGetText $ inHost
                txtPort   <- entryGetText $ inPort
                txtGameId <- entryGetText $ inGameId
                txtPlayer <- entryGetText $ inPlayer
                let cfg = IntermediateCfg {
                      host   = bar txtHost
                    , port   = bar txtPort
                    , conf   = Nothing
                    , gameId = bar txtGameId
                    , player = (let text = strip txtPlayer in if length text > 0 then Just $ read text else Nothing)
                    }

                forkIO $ do
                    res <- connect cfg
                    case res of
                        Right something -> do
                            swapMVar result $ Just something
                            postGUIAsync $ widgetHide window
                            postGUIAsync $ mainQuit
                        Left  msg      -> do
                            postGUIAsync $ labelSetMarkup label ("<span color=\"red\" weight=\"bold\">"++msg++"</span>")
                    postGUIAsync $ spinnerStop spinner
                    postGUIAsync $ widgetSetSensitive btConnect True
                
                spinnerStart spinner
                widgetSetSensitive btConnect False
    
    mainGUI
    takeMVar result
