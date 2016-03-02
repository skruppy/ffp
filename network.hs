import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString (send, recv)
import qualified Data.ByteString.Char8 as BS8
import Control.Exception
import Control.Monad.Error


main = client' 1357

client' :: Int -> IO ()
client' = client "localhost"

client :: String -> Int -> IO ()
client host port = withSocketsDo $ do
                addrInfo <- getAddrInfo Nothing (Just host) (Just $ show port)
                serverAddr <- (headByTrial addrInfo)
                sock <- socket (addrFamily serverAddr) Stream defaultProtocol
                connect sock (addrAddress serverAddr)
                putStrLn "successfully connected lalalala"
                msgSender sock
                sClose sock
                
headByTrial (x:xs) = 
                if xs == [] then    do  putStrLn "No valid address to connect to" 
                                        error "No valid address to connect to" 
                            else
                                    do  sock <- socket (addrFamily x) Stream defaultProtocol
                                        res <- try $ connect sock  (addrAddress x)
                                        case res of 
                                            Left (SomeException e) ->  headByTrial xs
                                            Right _ -> do   sClose sock
                                                            return x
                                            
--dummycode to be replaced with retrieving and writing to mvars.
msgSender :: Socket -> IO ()
msgSender sock = do
  rMsg <- recv sock 2048
  BS8.putStrLn rMsg
  msg <- BS8.getLine
  send sock msg
  if msg == BS8.pack "q" then putStrLn "Disconnected!" else msgSender sock