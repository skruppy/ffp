-- "THE BEER-WARE LICENSE" (Revision 42):
-- Albatrouss and <skruppy@onmars.eu> wrote this software. As long as you retain
-- this notice you can do whatever you want with this stuff. If we meet some
-- day, and you think this stuff is worth it, you can buy me a beer in return.
--                                                     -- Albatrouss and Skruppy

module Main where

import Sm as S
import Net
import AI as AI
import Network.Socket as NS
import System.IO
import Conf as C
import Conf.Args as CA
import Conf.Gui as CG
import Data.String.Utils
import System.Environment
import Util
import GameGUI as GG
import Control.Concurrent.MVar
import Control.Concurrent
import PP as PP
import Paths_funthello (version)
import Data.Version (showVersion)
import System.Console.ANSI

-- The name speaks for it self. Here you are looking at the beautiful main-l↺↺p.
(↺) hdl (SmEnd gameData winner) = putStrLn ("OK")
(↺) hdl (SmError msg)           = putStrLn ("FAILED: "++msg)
(↺) hdl (SmOk s o)              = do
    i <- converse hdl o
    let (s', io) = smStep s i
    maybio io
    (↺) hdl s'


play Nothing _ _ _ = putStrLn ("Missing hostname")
play _ Nothing _ _ = putStrLn ("Missing port")
play _ _ Nothing _ = putStrLn ("Missing game ID")
play (Just host') (Just port') (Just gameId') player' = do
    -- Get socket
    socket <- Net.connect host' port'
    -- Convert socket to unbuffered handle
    hdl    <- socketToHandle socket ReadWriteMode
    hSetBuffering hdl NoBuffering
    mVarField <- newEmptyMVar
    mVarGameData <- newEmptyMVar
    forkIO $ GG.createGameGUI mVarField mVarGameData
    -- Main loop
    let state = smCreate $ S.Cfg {
          S.gameId = gameId'
        , S.player = player'
        , S.ai     = \gameData field time -> ((AI.getNextMove mVarField mVarGameData field gameData) , Just $ PP.prettyPrint field)
        }
    input <- converse hdl []
    let (state', io) = smStep state input
    maybio io
    (↺) hdl state'
    
    -- So close!
    hClose hdl


align text n = text ++ (replicate (n - (length text)) ' ')

main :: IO ()
main = do
    -- Config from command line
    argsCfg <- CA.getCfg
    
    -- http://patorjk.com/software/taag/#p=display&f=Big&t=funthello
    setSGR [SetConsoleIntensity BoldIntensity]
    putStrLn "   ______           _   _          _ _"
    putStrLn "   |  ___|         | | | |        | | |"
    putStrLn "   | |_ _   _ _ __ | |_| |__   ___| | | ___"
    putStrLn "   |  _| | | | '_ \\| __| '_ \\ / _ \\ | |/ _ \\"
    putStrLn "   | | | |_| | | | | |_| | | |  __/ | | (_) |"
    putStrLn "   \\_|  \\__,_|_| |_|\\__|_| |_|\\___|_|_|\\___/"
    setSGR [Reset]
    putStrLn ""
    setSGR [SetColor Foreground Dull Green]
    putStr   "   Loading version "
    putStr   $ showVersion version
    putStr   " "
    setSGR [SetBlinkSpeed SlowBlink]
    putStr   "_"
    putStrLn ""
    setSGR [Reset]
    putStrLn ""

    progName <- getProgName
    guiCfg <- if endswith "-gui" progName
        then let cfg = mergeCfg [argsCfg, defaultCfg] in CG.getCfg cfg
        else return emptyCfg
    
    let cfg = mergeCfg [guiCfg, argsCfg, defaultCfg]
    
    play (C.host cfg) (C.port cfg) (C.gameId cfg) (C.player cfg)
