import Network.Socket as NS
import qualified Data.ByteString.Char8 as BS8
import Control.Exception
import Control.Monad.Error
-- import Sm
import System.IO
import System.Console.ANSI

hints = NS.defaultHints { NS.addrFlags = [NS.AI_ADDRCONFIG], NS.addrSocketType = NS.Stream }


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
    hdl <- client "localhost" 1357 -- get a connected socket
    netConverse hdl []
    netConverse hdl ["VERSION 1.0"]
    netConverse hdl ["ID asdf12364"]
    -- So close!
    hClose hdl


client host port = withSocketsDo $ do
    addrInfo   <- getAddrInfo (Just hints) (Just host) (Just $ show port)
    serverAddr <- (headByTrial addrInfo)
    sock       <- socket (addrFamily serverAddr) Stream defaultProtocol
    connect sock (addrAddress serverAddr)
    hdl        <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    putStrLn "successfully connected"
    return hdl


headByTrial [] = error "No valid address to connect to"

headByTrial (x:xs) = do
    sock <- socket (addrFamily x) Stream defaultProtocol
    res <- try $ connect sock  (addrAddress x)
    case res of
        Left (SomeException e) ->  headByTrial xs
        Right _ -> do sClose sock ; return x
