-- "THE BEER-WARE LICENSE" (Revision 42):
-- Albatrouss and <skruppy@onmars.eu> wrote this software. As long as you retain
-- this notice you can do whatever you want with this stuff. If we meet some
-- day, and you think this stuff is worth it, you can buy me a beer in return.
--                                                     -- Albatrouss and Skruppy

module Sm.Internal where

import Data.Array
import Data.List
import Data.Maybe
import Text.Regex.Posix
import Data.Ix
import Paths_funthello (version)
import Data.Version


data StepResult
    = SmOk QuallifiedState [String]
    | SmEnd GameData (Maybe Int)
    | SmError String
      deriving (Eq, Show)

data QuallifiedState
    = QuallifiedState State Cfg
      deriving (Eq, Show)

-- The following state definition is extremely confusing without propper type
-- names, therefore we define some:
type Major = Int
type Minor = Int
type GameType = String
type GameName = String
type MoveTime = Int
type WinnerId = Int

data BoardTransReason
    = Winner Int
    | Draw
    | Move MoveTime
      deriving (Eq, Show)

data State
    = StartState
    | VersionState     Major Minor
    | GameKindState    Major Minor
    | GameNameState    Major Minor GameType
    | PlayerNameState  Major Minor GameType GameName
    | PlayerStartState Major Minor GameType GameName        Int        PlayerItem
    | PlayerLineState  Major Minor GameType GameName (Array Int (Maybe PlayerItem)) Int
    | PlayerEndState   Major Minor GameType GameName (Array Int (Maybe PlayerItem))
    | IdleState        GameData
    | FieldStartState  GameData BoardTransReason
    | FieldLineState   GameData BoardTransReason      Int         Int  Int [[String]]
    | FieldEndState    GameData BoardTransReason                  Int  Int [[String]]
    | ThinkingState    GameData MoveTime                  (Array (Int, Int)  String)
    | MoveState        GameData
    | QuitState        GameData (Maybe WinnerId)
    | EndState         GameData (Maybe WinnerId)
    | ErrorState       String
      deriving (Eq, Show)

data Cfg = Cfg
    { gameId :: String
    , player :: Maybe Int
    , ai     :: GameData -> Array (Int, Int) String -> Int -> ( String, Maybe (IO ()) )
    }

instance Show Cfg where
    show (Cfg gameId player _) = "Cfg " ++ (show gameId) ++ " " ++ (show player)

instance Eq Cfg where
    (Cfg gameId1 player1 _) == (Cfg gameId2 player2 _) = gameId1 == gameId2 && player1 == player2

data PlayerItem = PlayerItem
    { playerName :: String
    , isReady    :: Bool
    , itsMe      :: Bool
    } deriving (Eq, Show)

data GameData = GameData
    { serverMajor :: Major
    , serverMinor :: Minor
    , gameType    :: GameType
    , gameName    :: GameName
    , players     :: Array Int PlayerItem
    } deriving (Eq, Show)


unexpectedInput expected ('-':input) = (ErrorState ("Server error:"++input++" (expected "++expected++")"), [], Nothing)
unexpectedInput expected (input)     = (ErrorState ("Protocoll error: Expected "++expected++", but got \""++input++"\""), [], Nothing)

constraintError msg input = (ErrorState (msg++". Caussed by \""++input++"\""), [], Nothing)

parseInput StartState cfg input =
    case map read xs of
       [major, minor] | major == clientMajor -> (VersionState major minor, ["VERSION "++(showVersion version)], Nothing)
       [major, minor]                        -> constraintError "Only protocol version 1 supported" input
       otherwise                             -> unexpectedInput "server banner" input
    where
        (_, _, _, xs) = input =~ "^\\+ MNM Gameserver v([0-9]+)\\.([0-9]+) accepting connections$" :: (String, String, String, [String])
        clientMajor = head $ versionBranch version


parseInput (VersionState major minor) cfg input =
    if input == "+ Client version accepted - please send Game-ID to join"
       then (GameKindState major minor, ["ID "++(gameId cfg)], Nothing)
       else unexpectedInput "client version to be accepted" input


parseInput (GameKindState major minor) cfg input =
    if length xs == 1
       then (GameNameState major minor (head xs), [], Nothing)
       else unexpectedInput "game kind" input
    where
        (_, _, _, xs) = (input =~ "^\\+ PLAYING (.+)$" :: (String, String, String, [String]))


parseInput (GameNameState major minor gameType) cfg input =
    if length xs == 1
       then (PlayerNameState major minor gameType (head xs), [maybe "PLAYER" (\nr -> "PLAYER "++(show nr)) (player cfg)], Nothing)
       else unexpectedInput "game name" input
    where
        (_, _, _, xs) = (input =~ "^\\+ (.+)$" :: (String, String, String, [String]))


parseInput (PlayerNameState major minor gameType gameName) cfg input =
    if length xs == 2
       then let [n, name] = xs in
           ( PlayerStartState
             major minor gameType gameName (read n)
             (PlayerItem {playerName = name, isReady = True, itsMe = True})
           , []
           , Nothing
           )
       else unexpectedInput "player info" input
    where
        (_, _, _, xs) = (input =~ "^\\+ YOU ([0-9])+ (.+)$" :: (String, String, String, [String]))


parseInput (PlayerStartState major minor gameType gameName myNr myName) cfg input =
    if length xs == 1
       then let n = (read $ head xs) - 1 in
           ( PlayerLineState
             major minor gameType gameName
             ( listArray
               (0, n)
               [if cnt == myNr then Just myName else Nothing | cnt <- [0..n]]
             )
             (n-1)
           , []
           , Nothing
           )
       else unexpectedInput "numer of oponents" input
    where
        (_, _, _, xs) = (input =~ "^\\+ TOTAL ([1-9][0-9]*)$" :: (String, String, String, [String]))


parseInput (PlayerLineState major minor gameType gameName players playerCnt) cfg input =
    case playerFromList xs of
         Just (nr, _) | nr > (snd $ bounds players) -> constraintError "Player ID out of bounds" input
         Just (nr, _) | (players!nr) /= Nothing -> constraintError "Player already defined" input
         Just (nr, player) -> (
             if playerCnt  > 0
                then PlayerLineState major minor gameType gameName (players // [(nr, player)]) (playerCnt-1)
                else PlayerEndState  major minor gameType gameName (players // [(nr, player)]),
             [], Nothing)
         otherwise -> unexpectedInput "oponent info" input
    where
        (_, _, _, xs) = (input =~ "^\\+ ([0-9]+) (.+) (0|1)$" :: (String, String, String, [String]))
        playerFromList [nr, name, rdy] = Just ((read nr), Just $ PlayerItem {playerName = name, isReady = (rdy == "1"), itsMe = False})
        playerFromList _                = Nothing


parseInput (PlayerEndState major minor gameType gameName players) cfg input =
    if input == "+ ENDPLAYERS"
       then (IdleState gameData, [], Nothing)
       else unexpectedInput "end of oponent list" input
    where
        gameData = GameData
            { serverMajor = major
            , serverMinor = minor
            , gameType    = gameType
            , gameName    = gameName
            , players     = listArray (bounds players) (catMaybes $ elems players)
            }


parseInput (IdleState gameData) cfg input =
    case input of
         "+ WAIT"     -> (IdleState gameData, ["OKWAIT"], Nothing)
         "+ GAMEOVER" -> (FieldStartState gameData Draw, [], Nothing)
         _ | length moveParts == 1     -> (FieldStartState gameData (Move $ read $ head moveParts), [], Nothing)
         _ | length gameoverParts == 2 ->
             let
                 [nr, name] = gameoverParts
                 nr' = read nr
                 players' = players gameData
             in
                 if inRange (bounds players') nr'
                 then
                     let
                         name' = playerName $ players' ! nr'
                     in
                         if name == name'
                         then (FieldStartState gameData (Winner 1), [], Nothing)
                         else constraintError "The winner name is not part of the players list" input
                 else constraintError "The winners player ID is out of bounds" input
         otherwise -> unexpectedInput "wait/gameover/move" input
    where
        (_, _, _, moveParts)     = (input =~ "^\\+ MOVE ([1-9][0-9]*)$"     :: (String, String, String, [String]))
        (_, _, _, gameoverParts) = (input =~ "^\\+ GAMEOVER ([0-9]+) (.+)$" :: (String, String, String, [String]))


parseInput (FieldStartState gameData boardTransReason) cfg input =
    if length xs == 2
       then let [x, y] = map read xs
            in (FieldLineState gameData boardTransReason x y y [], [], Nothing)
       else unexpectedInput "start of board" input
    where
        (_, _, _, xs) = (input =~ "^\\+ FIELD ([1-9][0-9]*),([1-9][0-9]*)$" :: (String, String, String, [String]))


parseInput (FieldLineState gameData boardTransReason x y curY field) cfg input =
    case xs of
        [n, s] -> let elements = words s in
            if length elements /= x then constraintError "Row has invalid number of elements" input else
            if read n /= curY       then constraintError "Unexpected row number" input else
            if curY > 1             then
                ( FieldLineState
                  gameData boardTransReason x y (curY-1) (elements:field)
                , []
                , Nothing
                ) else
            (FieldEndState gameData boardTransReason x y (elements:field), [], Nothing)
        otherwise -> unexpectedInput "board row" input
    where
       (_, _, _, xs) = (input =~ "^\\+ ([1-9][0-9]*) (.+)$" :: (String, String, String, [String]))


parseInput (FieldEndState gameData boardTransReason x y field) cfg input =
    if input == "+ ENDFIELD" then
        case boardTransReason of
            Move time     -> (ThinkingState gameData time f,    ["THINKING"], Nothing)
            Winner winner -> (QuitState gameData (Just winner), [],           Nothing)
            Draw          -> (QuitState gameData (Nothing),     [],           Nothing)
    else unexpectedInput "end of board" input
    where
        f = listArray ((1,1), (x,y)) (concat $ transpose field)


parseInput (ThinkingState gameData boardTransReason field) cfg input =
    if input == "+ OKTHINK"
       then (MoveState gameData, ["PLAY "++move], io)
       else unexpectedInput "OKTHINK" input
    where
        (move, io) = ai cfg gameData field boardTransReason


parseInput (MoveState gameData) cfg input =
    if input == "+ MOVEOK"
       then (IdleState gameData, [], Nothing)
       else unexpectedInput "acceptance of our move" input


parseInput (QuitState gameData winner) cfg input =
    if input == "+ QUIT"
       then (EndState gameData winner, [], Nothing)
       else unexpectedInput "quit" input


parseInput (EndState _ _) cfg input = error ("No input line should ever be parsed in the end state, but we still got \""++input++"\"")


parseInput (ErrorState _) cfg input = error ("No input line should ever be parsed in the error state, but we still got \""++input++"\"")


smCreate :: Cfg -> QuallifiedState
smCreate cfg = QuallifiedState StartState cfg


smStep (QuallifiedState s c) input =
    case parseInput s c input of
        (ErrorState msg           , _      , _ ) -> (SmError msg                        , Nothing)
        (EndState gameData winner , _      , _ ) -> (SmEnd gameData winner              , Nothing)
        (s'                       , output , io) -> (SmOk (QuallifiedState s' c) output , io     )
