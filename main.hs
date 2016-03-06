import Sm as S
import Net
import Network.Socket as NS
import System.IO
import Options.Applicative


-- This definition is so boring, we had to add a joke:
-- Chuck Norris can write Haskell... in assembler.
data Cfg = Cfg
    { host   :: String
    , port   :: String
    , conf   :: String
    , gameId :: String
    , player :: Maybe Int
    }


argParseCfg :: Parser Main.Cfg
argParseCfg = Main.Cfg <$> (
        option str
      $ long    "host"
     <> short   's'
     <> metavar "NAME"
     <> help    "Gameserver hostname"
     <> value   "sysprak.onmars.eu"
      )
    <*> (
        option str
      $ long    "port"
     <> short   'p'
     <> metavar "NAME/NUMBER"
     <> help    "Gameserver port number or service name"
     <> value   "1357"
      )
    <*> (
        option str
      $ long    "conf"
     <> short   'c'
     <> metavar "PATH"
     <> help    "Read configuration from file"
     <> value   "client.conf"
      )
    <*> (
        argument str
      $ metavar "GAMEID" -- "mSPb8GUCKxc"
     <> help    "Game ID"
      )
    <*> (
        optional $ argument auto
      $ metavar "PLAYER"
     <> help    "Player number (0, 1, ..)"
      )


-- The name speaks for it self. Here you are looking at a beautiful main-loop ↺.
(↺) hdl (SmEnd)       = putStrLn ("OK")
(↺) hdl (SmError msg) = putStrLn ("FAILED: "++msg)
(↺) hdl (SmOk s o)    = fmap (smStep s) (converse hdl o) >>= (↺) hdl


main :: IO ()
main = do
    cfg <- execParser $ info
        (helper <*> argParseCfg)
        (fullDesc <> header "AI for the sysprak gameserver.")
    
    -- Get socket
    socket <- Net.connect (host cfg) (port cfg)
    
    -- Convert socket to unbuffered handle
    hdl    <- socketToHandle socket ReadWriteMode
    hSetBuffering hdl NoBuffering
    
    -- Main loop
    let state = smCreate $ S.Cfg {
          S.gameId = Main.gameId cfg
        , S.player = Main.player cfg
        , S.ai     = \time -> \field -> "asd"
        }
    stepResult <- fmap (smStep state) (converse hdl [])
    (↺) hdl stepResult
    
    -- So close!
    hClose hdl
