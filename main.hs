import Sm as S
import Net
import Network.Socket as NS
import System.IO
import Conf as C
import Conf.Args as CA
import Conf.Gui as CG
import Data.String.Utils
import System.Environment

-- The name speaks for it self. Here you are looking at the beautiful main-l↺↺p.
(↺) hdl (SmEnd)       = putStrLn ("OK")
(↺) hdl (SmError msg) = putStrLn ("FAILED: "++msg)
(↺) hdl (SmOk s o)    = do
    i <- converse hdl o
    let (s', io) = smStep s i
    case io of
        Just x -> x
        _      -> return ()
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
    
    -- Main loop
    let state = smCreate $ S.Cfg {
          S.gameId = gameId'
        , S.player = player'
        , S.ai     = \gameData -> \field -> \time -> ("asd", Just $ putStrLn "asd")
        }
    input <- converse hdl []
    let (state', io) = smStep state input
--     io
    (↺) hdl state'
    
    -- So close!
    hClose hdl


main :: IO ()
main = do
    -- Config from command line
    argsCfg <- CA.getCfg
    
    progName <- getProgName
    guiCfg <- if endswith "-gui" progName
        then let cfg = mergeCfg [argsCfg, defaultCfg] in CG.getCfg cfg
        else return emptyCfg
    
    let cfg = mergeCfg [guiCfg, argsCfg, defaultCfg]
    
    play (C.host cfg) (C.port cfg) (C.gameId cfg) (C.player cfg)