-- file: ch27/syslogtcpclient.hs
module Sm where

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
import Text.Regex.Posix
import Data.Array



{-
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
-}

ai _ _ = "test"
testServerData = [
  "+ MNM Gameserver v1.0 accepting connections",-- VERSION 1.0
  "+ Client version accepted - please send Game-ID to join", -- ID ...
  "+ PLAYING Reversi",
  "+ The name of the game", -- PLAYER ...
  "+ TOTAL 2",
  "+ 1 Player 1 1",
  "+ ENDPLAYERS",
  "+ WAIT", -- OKWAIT
  "+ WAIT", -- OKWAIT
  "+ MOVE 3000",
  "+ FIELD 12,12",
  "+ 12 * * * * * * * * * * * *",
  "+ 11 * * * * * * * * * * * *",
  "+ 10 * * * * * * * * * * * *",
  "+ 9 * * * * * * * * * * * *",
  "+ 8 * * * * * * * * * * * *",
  "+ 7 * * * * * W B * * * * *",
  "+ 6 * * * * * B W * * * * *",
  "+ 5 * * * * * * * * * * * *",
  "+ 4 * * * * * * * * * * * *",
  "+ 3 * * * * * * * * * * * *",
  "+ 2 * * * * * * * * * * * *",
  "+ 1 * * * * * * * * * * * *",
  "+ ENDFIELD", -- THINKING
  "+ OKTHINK", -- PLAY ...
  "+ MOVEOK",
  "+ WAIT", -- OKWAIT
  "+ WAIT", -- OKWAIT
  "+ GAMEOVER 0 Your Name",
  "+ FIELD 12,12",
  "+ 12 * * * * * * * * * * * *",
  "+ 11 * * * * * * * * * * * *",
  "+ 10 * * * * * * * * * * * *",
  "+ 9 * * * * * * * * * * * *",
  "+ 8 * * * * * * * * * * * *",
  "+ 7 * * * * * W B * * * * *",
  "+ 6 * * * * * B W * * * * *",
  "+ 5 * * * * * * * * * * * *",
  "+ 4 * * * * * * * * * * * *",
  "+ 3 * * * * * * * * * * * *",
  "+ 2 * * * * * * * * * * * *",
  "+ 1 * * * * * * * * * * * *",
  "+ ENDFIELD",
  "+ QUIT"]

data State = ErrorState
    | StartState
    | VersionState
    | GameKindState
    | GameNameState
    | PlayerStartState
    | PlayerLineState  (Array Int PlayerItem) Int
    | PlayerEndState   (Array Int PlayerItem)
    | IdleState        (Array Int PlayerItem)
    | FieldStartState  (Array Int PlayerItem) (Maybe Int)
    | FieldLineState   (Array Int PlayerItem) (Maybe Int) Int Int Int [[String]]
    | FieldEndState    (Array Int PlayerItem) (Maybe Int) Int Int [[String]]
    | ThinkingState    (Array Int PlayerItem)        Int  (Array (Int, Int) String)
    | MoveState        (Array Int PlayerItem)
    | QuitState
    | EndState deriving (Eq, Show)

data Cfg =
    Cfg { gameId :: String,
          player :: Maybe Int} deriving (Eq, Show)

data PlayerItem =
    PlayerItem { playerName :: Maybe String,
                 isReady :: Bool} deriving (Eq, Show)

parseInput StartState cfg input =
    if major == Just 1
       then (VersionState, ["VERSION 1.42"])
       else (ErrorState, [])
    where
        (_, _, _, xs) = (input =~ "^\\+ MNM Gameserver v([0-9]+)\\.([0-9]+) accepting connections$" :: (String, String, String, [String]))
        l = map (\x -> Just (read x)) xs
        [major, minor] = if length l == 2 then l else [Nothing, Nothing]


parseInput VersionState cfg input =
    if input == "+ Client version accepted - please send Game-ID to join"
       then (GameKindState, ["ID "++(gameId cfg)])
       else (ErrorState, [])


parseInput GameKindState cfg input =
    if length xs == 1
       then (GameNameState, [])
       else (ErrorState, [])
    where
        (_, _, _, xs) = (input =~ "^\\+ PLAYING (.+)$" :: (String, String, String, [String]))


parseInput GameNameState cfg input =
    if length xs == 1
       then (PlayerStartState, [maybe "PLAYER" (\nr -> "PLAYER "++(show nr)) (player cfg)])
       else (ErrorState, [])
    where
        (_, _, _, xs) = (input =~ "^\\+ (.+)$" :: (String, String, String, [String]))


parseInput PlayerStartState cfg input =
    if length xs == 1
       then let n = (read $ head xs) - 1 in (
           PlayerLineState
           ( listArray (0, n)  (replicate (n+1) (PlayerItem {playerName = Nothing, isReady = True})) )
           (n-1), [])
       else (ErrorState, [])
    where
        (_, _, _, xs) = (input =~ "^\\+ TOTAL ([1-9][0-9]*)$" :: (String, String, String, [String]))


parseInput (PlayerLineState players playerCnt) cfg input =
    case playerFromList xs of
         Just (nr, _)      | nr > (snd $ bounds players) -> (ErrorState, [])
         Just (nr, _)      | (playerName (players!nr)) /= Nothing -> (ErrorState, [])
         Just (nr, player) -> (
             if playerCnt  > 0
                then PlayerLineState (players // [(nr, player)]) (playerCnt-1)
                else PlayerEndState  (players // [(nr, player)]),
             [])
         otherwise -> (ErrorState, [])
    where
        (_, _, _, xs) = (input =~ "^\\+ ([0-9]+) (.+) (0|1)$" :: (String, String, String, [String]))
        playerFromList (nr:name:rdy:[]) = Just ((read nr), PlayerItem {playerName = Just name, isReady = (rdy == "1")})
        playerFromList _                = Nothing


parseInput (PlayerEndState players) cfg input =
    if input == "+ ENDPLAYERS"
       then (IdleState players, [])
       else (ErrorState, [])


parseInput (IdleState players) cfg input =
    case input of
         "+ WAIT" -> (IdleState players, ["OKWAIT"])
         _ | length moveParts == 1     -> (FieldStartState players (Just $ read $ head moveParts), [])
         _ | length gameoverParts == 2 -> (FieldStartState players Nothing, [])
         otherwise -> (ErrorState, [])
    where
        (_, _, _, moveParts)     = (input =~ "^\\+ MOVE ([1-9][0-9]*)$"          :: (String, String, String, [String]))
        (_, _, _, gameoverParts) = (input =~ "^\\+ GAMEOVER ([0-9]+) (.+)$" :: (String, String, String, [String]))


parseInput (FieldStartState players time) cfg input =
    if length xs == 2
       then let [x, y] = map read xs
            in (FieldLineState players time x y y [], [])
       else (ErrorState, [])
    where
        (_, _, _, xs) = (input =~ "^\\+ FIELD ([1-9][0-9]*),([1-9][0-9]*)$" :: (String, String, String, [String]))


parseInput (FieldLineState players time x y curY field) cfg input =
    case xs of
        n:s:[] -> let elements = words s in
            if length elements /= x then (ErrorState, []) else
            if read n /= curY then (ErrorState, []) else
            if curY > 1 then (FieldLineState players time x y (curY-1) (elements:field), []) else
            (FieldEndState players time x y (elements:field), [])
        otherwise -> (ErrorState, [])
    where
       (_, _, _, xs) = (input =~ "^\\+ ([1-9][0-9]*) (.+)$" :: (String, String, String, [String]))


parseInput (FieldEndState players time x y field) cfg input =
    if input == "+ ENDFIELD" then
        case time of
            Just t    -> (ThinkingState players t f, ["THINKING"])
            otherwise -> (QuitState, [])
    else (ErrorState, [])
    where
        f = listArray ((1,1), (x,y)) $ concat field


parseInput (ThinkingState players time field) cfg input =
    if input == "+ OKTHINK"
       then (MoveState players, ["PLAY "++move])
       else (QuitState, [])
    where
        move = ai time field


parseInput (MoveState players) cfg input =
    if input == "+ MOVEOK"
       then (IdleState players, [])
       else (QuitState, [])


parseInput QuitState cfg input =
    if input == "+ QUIT"
       then (EndState, [])
       else (ErrorState, [])


parseInput ErrorState cfg input = error "No input line should ever be parsed in the error state"


smStep (input:inputs) oldState cfg =
    if newState /= ErrorState then smStep inputs newState cfg else newState
    where
        (newState, toSend) = parseInput oldState cfg input


sm inputs cfg = smStep inputs StartState cfg
-- 
-- main =
--     sm (net cfg) cfg
--     where
--         cfg = parseCfg args



