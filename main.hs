-- file: ch27/syslogtcpclient.hs
import Data.Bits
import Network.Socket
import Network.BSD
import Network.Simple.TCP
import Data.List
import System.IO
import qualified Network.Socket                 as NS
import           Control.Monad.IO.Class         (MonadIO(liftIO))
import qualified Control.Exception              as E
import Control.Monad.Error



-- https://hackage.haskell.org/package/network-simple-0.4.0.4/docs/src/Network-Simple-TCP.html#connect
connectSock2 ::  MonadIO m => NS.HostName -> NS.ServiceName -> Maybe (m (NS.Socket))
connectSock2 host port =
    findConnection $
    liftIO $
    NS.getAddrInfo (Just hints) (Just host) (Just port)
    where
        hints = NS.defaultHints { NS.addrFlags = [NS.AI_ADDRCONFIG], NS.addrSocketType = NS.Stream }
        
        -- https://hackage.haskell.org/package/network-simple-0.4.0.4/docs/src/Network-Simple-TCP.html#newSocket
        newSocket :: NS.AddrInfo -> NS.Socket
        newSocket ai = NS.socket (NS.addrFamily ai) (NS.addrSocketType ai) (NS.addrProtocol ai)
        
        tryToConnect :: NS.AddrInfo -> NS.Socket
        tryToConnect ai = do
            E.bracketOnError
                -- At the beginning: IO a
                (newSocket ai)
                
                -- On error: (a -> IO b)
                (closeSock)
                
                -- "Main" operation: (a -> IO c)
                (\s -> do
                    NS.connect s (NS.addrAddress ai)
                    return s)
        
        findConnection :: [NS.AddrInfo] -> Maybe (NS.Socket)
        findConnection nil = Nothing
        findConnection (ai:ais) = Just (tryToConnect ai) `E.catch` (findConnection ais)


--    liftIO $
main = do
	connectSock2 "foo.lan" "http"

-- syslog :: SyslogHandle -> Facility -> Priority -> String -> IO ()
-- syslog syslogh fac pri msg =
--     do hPutStrLn (slHandle syslogh) sendmsg
--        -- Make sure that we send data immediately
--        hFlush (slHandle syslogh)
--     where code = makeCode fac pri
--           sendmsg = "<" ++ show code ++ ">" ++ (slProgram syslogh) ++
--                     ": " ++ msg
