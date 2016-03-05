import Sm
import Net
import Network.Socket as NS
import System.IO



(↺) hdl (SmEnd)       = putStrLn "OK"
(↺) hdl (SmError msg) = putStrLn ("FAILED: "++msg)
(↺) hdl (SmOk s o)    = fmap (smStep s) (netConverse hdl o) >>= (↺) hdl


main :: IO ()
main = do
    -- Get socket
    socket <- netConnect "sysprak.onmars.eu" "1357"
    
    -- Convert socket to unbuffered handle
    hdl    <- socketToHandle socket ReadWriteMode
    hSetBuffering hdl NoBuffering
    
    let s = smCreate $ Cfg { gameId = "mSPb8GUCKxc", player = Nothing }
    x <- fmap (smStep s) (netConverse hdl [])
    (↺) hdl x
    
    -- So close!
    hClose hdl
