-- "THE BEER-WARE LICENSE" (Revision 42):
-- Albatrouss and <skruppy@onmars.eu> wrote this software. As long as you retain
-- this notice you can do whatever you want with this stuff. If we meet some
-- day, and you think this stuff is worth it, you can buy me a beer in return.
--                                                     -- Albatrouss and Skruppy

module Net(Net.connect,Net.listen,speak,converse) where

import Network.Socket as NS
import Control.Exception
import System.IO
import System.Console.ANSI


-- *Knock Knock* "Who's there?" "Denial of Service Attack" "Den...?"
knockKnock :: [AddrInfo] -> IO (Either String Socket)
knockKnock [] = do
    putStrLn "Oh noes, I can't connect to the server. The washing machine ate all my sock(et)s"
    return $ Left "Can't connect"

knockKnock (x:xs) = do
    putStr $ "Connecting to "++(show $ addrAddress x)
    sock <- socket (addrFamily x) Stream defaultProtocol
    res  <- try $ NS.connect sock (addrAddress x) :: IO (Either SomeException ())
    case res of
        Right _ -> do
            putStrLn " ✔"
            return $ Right sock
        Left _ -> do
            putStrLn " ✘"
            sClose sock
            knockKnock xs


--         /String     /String
connect :: HostName -> ServiceName -> IO (Either String Socket)
connect host port = withSocketsDo $ do
    res <- try $ getAddrInfo (Just hints) (Just host) (Just port) :: IO (Either SomeException [AddrInfo])
    case res of
        Right addrInfo -> do
            sock <- knockKnock addrInfo
            return sock
        Left exception -> do
            return $ Left $ "Can't resilve host or port:\n" ++ (show exception)
    where
        hints = NS.defaultHints
            { NS.addrFlags      = [AI_ADDRCONFIG,AI_V4MAPPED]
            , NS.addrSocketType = NS.Stream
            }


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





