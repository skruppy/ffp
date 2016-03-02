import Network.Socket as NS
import qualified Data.ByteString.Char8 as BS8
import Control.Exception
import Control.Monad.Error


main = do sock <- client "localhost" 1357
          msg <- recv sock 100
          putStrLn msg
          send sock "VERSION 1.0\n"
          --putStrLn $ show bytesSent
          msg1 <- recv sock 100
          putStr msg1
          putStrLn ""
          bytesSent <- send sock "ID asdf1234\n"
          --putStrLn $ show bytesSent
          msg2 <- recv sock 2048
          putStr msg2 
          putStrLn ""
          msg3 <- recv sock 2048
          putStrLn msg3
          msg4 <- recv sock 2048
          putStrLn msg4
          sClose sock

client :: String -> Int -> IO (Socket)
client host port = withSocketsDo $ do
                addrInfo <- getAddrInfo (Just hints) (Just host) (Just $ show port)
                serverAddr <- (headByTrial addrInfo)
                sock <- socket (addrFamily serverAddr) Stream defaultProtocol
                connect sock (addrAddress serverAddr)
                putStrLn "successfully connected lalalala"
                return sock
                --msgSender sock
                --sClose sock
                
headByTrial (x:xs) = 
                if (x:xs) == [] then    do  putStrLn "No valid address to connect to" 
                                            error "No valid address to connect to" 
                            else
                                    do  sock <- socket (addrFamily x) Stream defaultProtocol
                                        res <- try $ connect sock  (addrAddress x)
                                        case res of 
                                            Left (SomeException e) ->  headByTrial xs
                                            Right _ -> do   sClose sock
                                                            return x
                                                            
hints = NS.defaultHints { NS.addrFlags = [NS.AI_ADDRCONFIG], NS.addrSocketType = NS.Stream }



--dummycode to be replaced with retrieving and writing to mvars.
--msgSender :: Socket -> IO ()
--msgSender sock = do
--  rMsg <- recv sock 2048
--  putStrLn rMsg
--  msg <- getLine
--  send sock msg
--  if msg == BS8.pack "q" then putStrLn "Disconnected!" else msgSender sock