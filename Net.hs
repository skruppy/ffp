module Net where

import Network.Socket as NS
import Control.Exception
import System.IO
import System.Console.ANSI



netKnockKnock :: [AddrInfo] -> IO Socket
netKnockKnock [] = do
    putStrLn "Oh noes, I can't connect to the server. The washing machine ate all my sock(et)s"
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


--            /String     /String
netConnect :: HostName -> ServiceName -> IO Socket
netConnect host port = withSocketsDo $ do
    addrInfo <- getAddrInfo (Just hints) (Just host) (Just port)
    sock     <- netKnockKnock addrInfo
    return sock
    where
        hints = NS.defaultHints {
            NS.addrFlags      = [AI_ADDRCONFIG,AI_V4MAPPED],
            NS.addrSocketType = NS.Stream }


netListen :: Handle -> IO String
netListen hdl = do
    msg <- hGetLine hdl
    putStr "S: "
    
    setSGR [SetColor Foreground Vivid Magenta, SetConsoleIntensity BoldIntensity]
    putStrLn msg
    setSGR [Reset]
    
    return msg


netSpeak :: Handle -> String -> IO ()
netSpeak hdl line = do
    putStr "C: "
    
    setSGR [SetColor Foreground Vivid Cyan, SetConsoleIntensity BoldIntensity]
    putStrLn line
    setSGR [Reset]
    
    hPutStrLn hdl line


netConverse :: Handle -> [String] -> IO String
netConverse hdl [] = netListen hdl

netConverse hdl (line:remaining) = do
    netSpeak hdl line
    netConverse hdl remaining





