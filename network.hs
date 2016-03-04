import Network.Socket as NS
import Control.Exception
import Control.Monad.Error
-- import Sm
import System.IO
import System.Console.ANSI



netKnockKnock [] = do
    putStrLn "Noooose, I can't connect to the server. The washing machine ate all my socks"
    error "No valid address to connect to"

netKnockKnock (x:xs) = do
    putStr $ "Connecting to "++(show $ addrAddress x)
    sock <- socket (addrFamily x) Stream defaultProtocol
    res  <- try $ connect sock (addrAddress x) :: IO (Either SomeException ())
    case res of
        Left _ -> do
            putStrLn " ✘"
            sClose sock
            netKnockKnock xs
        Right _ -> do
            putStrLn " ✔"
            return sock


netConnect host port = withSocketsDo $ do
    addrInfo <- getAddrInfo Nothing (Just host) (Just port)
    sock     <- netKnockKnock addrInfo
    return sock
    where
        hints = NS.defaultHints {
            NS.addrFlags      = [NS.AI_ADDRCONFIG],
            NS.addrSocketType = NS.Stream }


netListen hdl = do
    msg <- hGetLine hdl
    putStr "S: "
    
    setSGR [SetColor Foreground Vivid Magenta, SetConsoleIntensity BoldIntensity]
    putStrLn msg
    setSGR [Reset]
    
    return msg


netSpeak hdl line = do
    putStr "C: "
    
    setSGR [SetColor Foreground Vivid Cyan, SetConsoleIntensity BoldIntensity]
    putStrLn line
    setSGR [Reset]
    
    hPutStrLn hdl line


netConverse hdl [] = netListen hdl

netConverse hdl (line:remaining) = do
    netSpeak hdl line
    netConverse hdl remaining


main = do
    -- Get socket
    socket <- netConnect "sysprak.onmars.eu" "1357"
    
    -- Convert socket to unbuffered handle
    hdl    <- socketToHandle socket ReadWriteMode
    hSetBuffering hdl NoBuffering
    
    netConverse hdl []
    netConverse hdl ["VERSION 1.0"]
    netConverse hdl ["ID asdf12364"]
    
    -- So close!
    hClose hdl


