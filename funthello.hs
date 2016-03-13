-- "THE BEER-WARE LICENSE" (Revision 42):
-- Albatrouss and <skruppy@onmars.eu> wrote this software. As long as you retain
-- this notice you can do whatever you want with this stuff. If we meet some
-- day, and you think this stuff is worth it, you can buy me a beer in return.
--                                                     -- Albatrouss and Skruppy

module Main where

import AI as AI
import Conf as C
import Conf.Args as CA
import Conf.Gui as CG
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Data.Array
import Data.Maybe
import Data.String.Utils
import Data.Version (showVersion)
import GameGUI as GG
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Windows.MessageDialog
import Net
import Network.Socket as NS
import PP as PP
import Paths_funthello (version)
import Sm as S
import System.Console.ANSI
import System.Environment
import System.Exit
import System.IO
import Util



-- The name speaks for it self. Here you are looking at the beautiful main-l↺↺p.
(↺) :: Handle -> StepResult -> IO (Either String (GameData, Maybe Int, Board))
(↺) hdl (SmEnd gameData winner board) = return $ Right (gameData, winner, board)
(↺) hdl (SmError msg)                 = return $ Left msg
(↺) hdl (SmOk s o)                    = do
    i <- converse hdl o
    let (s', io) = smStep s i
    io
    (↺) hdl s'


play :: Cfg -> NS.Socket -> IO (Either String (GameData, Maybe Int, Board))
play cfg socket = do
    -- Convert socket to unbuffered handle
    hdl <- socketToHandle socket ReadWriteMode
    hSetBuffering hdl NoBuffering
    
    -- Main loop
    res <- (↺) hdl $ smCreate cfg
    
    -- So close!
    hClose hdl
    return res


finalizeCfg :: IntermediateCfg -> IO (Either String (String, Maybe Int, NS.Socket))
finalizeCfg IntermediateCfg { C.host   = Nothing } = return $ Left "Missing hostname"
finalizeCfg IntermediateCfg { C.port   = Nothing } = return $ Left "Missing port"
finalizeCfg IntermediateCfg { C.gameId = Nothing } = return $ Left "Missing game ID"
finalizeCfg IntermediateCfg
    { C.host   = Just host'
    , C.port   = Just port'
    , C.gameId = Just gameId'
    , C.player =      player'
    }
    = do
    res <- Net.connect host' port'
    case res of
        Right socket -> return $ Right (gameId', player', socket)
        Left msg     -> return $ Left msg


printGameData :: String -> GameData -> IO ()
printGameData gameId gameData = do
    putStrLn $ "Server Version: "++(show $ serverMajor gameData)++"."++(show $ serverMinor gameData)
    putStrLn $ "     Game Type: "++(gameType gameData)
    putStrLn $ "     Game Name: "++(gameName gameData)
    putStrLn $ "       Game Id: "++(gameId)++" (Watch: http://sysprak.priv.lab.nm.ifi.lmu.de/sysprak/"++(gameType gameData)++"/"++(gameId)++")"
    putStrLn $ ""
    putStrLn $ "Players:"
    mapM_ printPlayer $ assocs $ players gameData
    
    where
        printPlayer (i, (PlayerItem playerName' isReady' itsMe')) = do
            putStr $ "   "++(show i)++": "
            setSGR $
                if      itsMe'   then [SetColor Foreground Vivid Green, SetConsoleIntensity BoldIntensity]
                else if isReady' then [SetColor Foreground Dull Green]
                                 else [SetColor Foreground Vivid Yellow]
            putStr playerName'
            setSGR [Reset]
            
            putStrLn $
                if      itsMe'   then " (You)"
                else if isReady' then " (Ready)"
                                 else " (Play: http://sysprak.priv.lab.nm.ifi.lmu.de/sysprak/"++(gameType gameData)++"/"++(gameId)++"#"++(show i)++")"


{-== GUI MODE ==-}
guiPreAi :: Gui -> String -> GameData -> Array (Int, Int) String -> Int -> IO ()
guiPreAi gui gameId gameData board time = do
    putStrLn $ "Now I can think "++(show time)++"ms about the best move on the following board:"
    prettyPrint board
    updateBoard gui board


guiAi :: Gui -> String -> GameData -> Array (Int,Int) String -> Int -> (String, IO () )
guiAi gui gameId gameData board time =
    (move, do
        putStrLn $ "I decided to do: "++move
        prettyPrint nextBoard
        updateBoard gui nextBoard
    )
    where
        (move, nextBoard) = AI.getNextMove board gameData


startMainGui :: (String, Maybe Int, NS.Socket) -> IO Bool
startMainGui (gameId', player', socket) = do
    gui <- guiNew
    
    forkIO $ do
        let cfg = S.Cfg {
              S.gameId           = gameId'
            , S.player           = player'
            , S.gameDataComplete = updateGameData gui
            , S.preAi            = guiPreAi gui
            , S.ai               = guiAi gui
            }
        
        -- Do the game.
        res <- play cfg socket
        
        -- Check the result of the game.
        case res of
            -- The game ended without protocol issues.
            Right (gameData, winner, board) -> do
                -- Print the final board
                prettyPrint board
                updateBoard gui board
                
                case winner of
                    -- The game ended with a WINNER (and a looser)
                    Just winnerNr -> do
                       let winnerRecord = (players gameData) ! winnerNr
                       
                       if itsMe winnerRecord
                           -- We won
                           then do
                               let msg = "You won"
                               putStrLn msg
                               showGuiMsg gui MessageInfo msg
                           -- We lost
                           else do
                               let msg = (playerName winnerRecord) ++ " won"
                               putStrLn msg
                               showGuiMsg gui MessageError msg
                    
                    -- The game ended in a DRAW
                    Nothing -> do
                        let msg = "The game ended in a draw"
                        putStrLn msg
                        showGuiMsg gui MessageInfo msg
            
            -- ERROR in game. We got some protocol issues.
            Left msg -> do
                putStrLn msg
                showGuiMsg gui MessageError msg
    
    runGui gui
    
    -- The GUI always exits successfully (to big to fail :)
    return True


guiMode :: IntermediateCfg -> IO Bool
guiMode cfg = do
    -- Init GTK once before any other GTK call
    initGUI
    
    -- Try to get a config from the user via GUI
    cfg' <- CG.getCfg cfg finalizeCfg
    
    -- If we got a valid config, start the main GUI, otherwise return successfully
    maybe (return True) startMainGui cfg'


{-== CONSOLE MODE ==-}
consoleGameDataComplete :: String -> GameData -> IO ()
consoleGameDataComplete gameId gameData = do
    printGameData gameId gameData


consolePreAi :: String -> GameData -> Array (Int,Int) String -> Int -> IO ()
consolePreAi gameId gameData board time = do
    putStrLn $ "Now I can think "++(show time)++"ms about the best move on the following board:"
    prettyPrint board


consoleAi :: String -> GameData -> Array (Int,Int) String -> Int -> (String ,IO ())
consoleAi gameId gameData board time =
    (move, do
        putStrLn $ "I decided to do: "++move 
        prettyPrint nextBoard
    )
    where
        (move, nextBoard) = AI.getNextMove board gameData


consoleMode :: IntermediateCfg -> IO Bool
consoleMode cfg = do
    res <- finalizeCfg cfg
    case res of
        Right (gameId', player', socket) -> do
            let cfg' = S.Cfg {
                  S.gameId           = gameId'
                , S.player           = player'
                , S.gameDataComplete = consoleGameDataComplete
                , S.preAi            = consolePreAi
                , S.ai               = consoleAi
                }
                
            -- Do the game.
            res <- play cfg' socket
            
            -- Check the result of the game.
            case res of
                -- The game ended without protocol issues.
                Right (gameData, winner, board) -> do
                    -- Print the final board
                    prettyPrint board
                    
                    case winner of
                        -- The game ended with a WINNER (and a looser)
                        Just winnerNr -> do
                            let winnerRecord = (players gameData) ! winnerNr
                            
                            if itsMe winnerRecord
                                -- We won
                                then putStrLn "You won"
                                
                                -- We lost
                                else putStrLn $ (playerName winnerRecord) ++ " won"
                        
                        -- The game ended in a DRAW
                        Nothing -> do
                            putStrLn "The game ended in a draw"
                    
                    -- Exit successfully
                    return True
                
                -- Error in game
                Left msg -> do
                    putStrLn msg
                    return False
        
        -- Configuration error
        Left msg -> do
            putStrLn msg
            return False


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

    let cfg = mergeCfg [argsCfg, defaultCfg]
    progName <- getProgName
    
    ret <- if endswith "-gui" progName
    then guiMode cfg
    else consoleMode cfg
    
    if ret
    then exitSuccess
    else exitFailure
