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
import Data.String.Utils
import Data.Array
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
import Data.Maybe



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


{-== CONSOLE MODE ==-}
blaa gameId' player' socket = do
    mVarField <- newEmptyMVar
    mVarGameData <- newEmptyMVar
    
    let ai = \gameId gameData board time -> (AI.getNextMove board gameData, do
            tryPutMVar mVarField board
            tryPutMVar mVarGameData gameData
            PP.prettyPrint board)
    
    forkIO $ do
        let cfg = S.Cfg {
              S.gameId           = gameId'
            , S.player           = player'
            , S.gameDataComplete = \_ _ -> return ()
            , S.preAi            = \_ _ _ _ -> return ()
            , S.ai               = ai
            }
        res <- play cfg socket
        case res of
            Right (gameData, winner, board) -> do
                tryPutMVar mVarField board
                tryPutMVar mVarGameData gameData
                PP.prettyPrint board
                case winner of
                     Just winner' -> do
                        let winner'' = (players gameData) ! winner'
                        if itsMe winner''
                            then putStrLn "You won"
                            else putStrLn $ (playerName winner'') ++ " won"
                     Nothing -> do
                         putStrLn "The game ended in a draw"
            
            -- Error in game
            Left msg -> do
                putStrLn msg
        return ()
    
    GG.createGameGUI mVarField mVarGameData

guiMode cfg = do
    initGUI
    cfg' <- CG.getCfg cfg (\cfg' -> finalizeCfg cfg')
    
    case cfg' of
        Just (gameId', player', socket) -> do
            blaa gameId' player' socket
            return True
        Nothing -> return True


{-== CONSOLE MODE ==-}
consoleGameDataComplete gameId gameData = do
    printGameData gameId gameData


consolePreAi gameId gameData board time = do
    putStrLn $ "Now I can think "++(show time)++"ms about the best move on the following board:"
    PP.prettyPrint board


consoleAi gameId gameData board time =
    (move, do
        putStrLn $ "I decided to do: "++move
    )
    where
        move = AI.getNextMove board gameData

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
            res <- play cfg' socket
            
            case res of
                Right (gameData, winner, board) -> do
                    PP.prettyPrint board
                    case winner of
                         Just winner' -> do
                            let winner'' = (players gameData) ! winner'
                            if itsMe winner''
                                then putStrLn "You won"
                                else putStrLn $ (playerName winner'') ++ " won"
                         Nothing -> do
                             putStrLn "The game ended in a draw"
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

