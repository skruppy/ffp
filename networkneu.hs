import Network
import System.IO (hGetLine,hClose,hPutStrLn,hSetBuffering,BufferMode(..),Handle,stdout)

host = "localhost"
port = 1357

main = do h <- startConnection host port --get a handle using the startConnection function
          msg <- hGetLine h -- use hGetLine with the handle to receive a Line from the Server
          putStrLn msg 
          hPutStrLn h "VERSION 1.0" -- use hPutStrLn to post a line to the Server (already contains a newline character at the end)
          msg1 <- hGetLine h
          putStrLn msg1
          hClose h -- close the handle 

startConnection :: HostName -> PortNumber -> IO Handle
startConnection host port = withSocketsDo $ do
                                hSetBuffering stdout NoBuffering
                                h <- connectTo host (PortNumber port)
                                hSetBuffering h LineBuffering
                                putStrLn $ "Connected to: " ++ host ++ ":" ++ show port
                                return h
                                