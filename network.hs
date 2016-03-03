import Network.Socket as NS
import qualified Data.ByteString.Char8 as BS8
import Control.Exception
import Control.Monad.Error

hints = NS.defaultHints { NS.addrFlags = [NS.AI_ADDRCONFIG], NS.addrSocketType = NS.Stream }

main = do sock <- client "localhost" 1357 -- get a connected socket
          msg <- recv sock 100 -- to receive pass the socket and a bytelength
          putStrLn msg
          bytesSent <- send sock "VERSION 1.0\n" -- to send pass the socket and a message -> returns bytes sent
          putStrLn $ show bytesSent
          msg1 <- recv sock 100
          putStr msg1
          --putStrLn ""
          bytesSent1 <- send sock "ID asdf12364\n"
          putStrLn $ show bytesSent1
          msg2 <- recv sock 2048
          putStr msg2 
         -- putStrLn ""
          msg3 <- recv sock 2048
          putStr msg3
        --  putStrLn ""
          msg4 <- recv sock 2048
          putStr msg4
       --   putStrLn ""
          sClose sock

client :: String -> Int -> IO (Socket)
client host port = withSocketsDo $ do
                addrInfo <- getAddrInfo (Just hints) (Just host) (Just $ show port)
                serverAddr <- (headByTrial addrInfo)
                sock <- socket (addrFamily serverAddr) Stream defaultProtocol
                connect sock (addrAddress serverAddr)
                putStrLn "successfully connected"
                return sock
                --msgSender sock
                --sClose sock
                
headByTrial (x:xs) = 
                if (x:xs) == [] then    do  putStrLn "No valid address to connect to" -- empty list
                                            error "No valid address to connect to" 
                            else
                                    do  sock <- socket (addrFamily x) Stream defaultProtocol
                                        res <- try $ connect sock  (addrAddress x)
                                        case res of 
                                            Left (SomeException e) ->  headByTrial xs
                                            Right _ -> do   sClose sock
                                                            return x -- return the first good address
                                                            
