module Net(Net.connect,Net.listen,speak,converse) where

import Network.Socket as NS
import Control.Exception
import System.IO
import System.Console.ANSI


-- *Knock Knock* "Who's there?" "Denial of Service Attack" "Den...?"
knockKnock :: [AddrInfo] -> IO Socket
knockKnock [] = do
    putStrLn "Oh noes, I can't connect to the server. The washing machine ate all my sock(et)s"
    error "No valid address to connect to"

knockKnock (x:xs) = do
    putStr $ "Connecting to "++(show $ addrAddress x)
    sock <- socket (addrFamily x) Stream defaultProtocol
    res  <- try $ NS.connect sock (addrAddress x) :: IO (Either SomeException ())
    case res of
        Left _ -> do
            putStrLn " ✘"
            sClose sock
            knockKnock xs
        Right _ -> do
            putStrLn " ✔"
            return sock


--         /String     /String
connect :: HostName -> ServiceName -> IO Socket
connect host port = withSocketsDo $ do
    addrInfo <- getAddrInfo (Just hints) (Just host) (Just port)
    sock     <- knockKnock addrInfo
    return sock
    where
        hints = NS.defaultHints {
            NS.addrFlags      = [AI_ADDRCONFIG,AI_V4MAPPED],
            NS.addrSocketType = NS.Stream }


listen :: Handle -> IO String
listen hdl = do
    msg <- hGetLine hdl
    putStr "S: "
    
    setSGR [SetColor Foreground Vivid Magenta, SetConsoleIntensity BoldIntensity]
    putStrLn msg
    setSGR [Reset]
    
    return msg


speak :: Handle -> String -> IO ()
speak hdl line = do
    putStr "C: "
    
    setSGR [SetColor Foreground Vivid Cyan, SetConsoleIntensity BoldIntensity]
    putStrLn line
    setSGR [Reset]
    
    hPutStrLn hdl line


converse :: Handle -> [String] -> IO String
converse hdl [] = Net.listen hdl

converse hdl (line:remaining) = do
    speak hdl line
    converse hdl remaining





