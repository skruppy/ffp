module Sm (StepResult(..),Cfg(..),smCreate,smStep) where

import Data.Array
import Data.List
import Text.Regex.Posix


ai _ _ = "test"

data StepResult
    = SmOk QuallifiedState [String]
    | SmEnd
    | SmError String
      deriving (Eq, Show)

data QuallifiedState
    = QuallifiedState State Cfg
      deriving (Eq, Show)

data State
    = ErrorState
    | StartState
    | VersionState
    | GameKindState
    | GameNameState
    | PlayerNameState
    | PlayerStartState        Int       PlayerItem
    | PlayerLineState  (Array Int (Maybe PlayerItem)) Int
    | PlayerEndState   (Array Int (Maybe PlayerItem))
    | IdleState        (Array Int (Maybe PlayerItem))
    | FieldStartState  (Array Int (Maybe PlayerItem)) (Maybe Int)
    | FieldLineState   (Array Int (Maybe PlayerItem)) (Maybe Int) Int Int Int [[String]]
    | FieldEndState    (Array Int (Maybe PlayerItem)) (Maybe Int) Int Int [[String]]
    | ThinkingState    (Array Int (Maybe PlayerItem))        Int  (Array (Int, Int) String)
    | MoveState        (Array Int (Maybe PlayerItem))
    | QuitState
    | EndState
      deriving (Eq, Show)

data Cfg = Cfg
    { gameId :: String
    , player :: Maybe Int
    } deriving (Eq, Show)

data PlayerItem = PlayerItem
    { playerName :: String
    , isReady    :: Bool
    , itsMe      :: Bool
    } deriving (Eq, Show)


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
       then (PlayerNameState, [maybe "PLAYER" (\nr -> "PLAYER "++(show nr)) (player cfg)])
       else (ErrorState, [])
    where
        (_, _, _, xs) = (input =~ "^\\+ (.+)$" :: (String, String, String, [String]))


parseInput PlayerNameState cfg input =
    if length xs == 2
       then let [n, name] = xs in (PlayerStartState (read n) (PlayerItem {playerName = name, isReady = True, itsMe = True}), [])
       else (ErrorState, [])
    where
        (_, _, _, xs) = (input =~ "^\\+ YOU ([0-9])+ (.+)$" :: (String, String, String, [String]))


parseInput (PlayerStartState myNr myName) cfg input =
    if length xs == 1
       then let n = (read $ head xs) - 1 in (
           PlayerLineState
           ( listArray (0, n)  [if cnt == myNr then Just myName else Nothing | cnt <- [0..n]] )
           (n-1), [])
       else (ErrorState, [])
    where
        (_, _, _, xs) = (input =~ "^\\+ TOTAL ([1-9][0-9]*)$" :: (String, String, String, [String]))


parseInput (PlayerLineState players playerCnt) cfg input =
    case playerFromList xs of
         Just (nr, _) | nr > (snd $ bounds players) -> (ErrorState, [])
         Just (nr, _) | (players!nr) /= Nothing -> (ErrorState, [])
         Just (nr, player) -> (
             if playerCnt  > 0
                then PlayerLineState (players // [(nr, player)]) (playerCnt-1)
                else PlayerEndState  (players // [(nr, player)]),
             [])
         otherwise -> (ErrorState, [])
    where
        (_, _, _, xs) = (input =~ "^\\+ ([0-9]+) (.+) (0|1)$" :: (String, String, String, [String]))
        playerFromList [nr, name, rdy] = Just ((read nr), Just $ PlayerItem {playerName = name, isReady = (rdy == "1"), itsMe = False})
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
        [n, s] -> let elements = words s in
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
        f = listArray ((1,1), (x,y)) (concat $ transpose field)


parseInput (ThinkingState players time field) cfg input =
    if input == "+ OKTHINK"
       then (MoveState players, ["PLAY "++move])
       else (ErrorState, [])
    where
        move = ai time field


parseInput (MoveState players) cfg input =
    if input == "+ MOVEOK"
       then (IdleState players, [])
       else (ErrorState, [])


parseInput QuitState cfg input =
    if input == "+ QUIT"
       then (EndState, [])
       else (ErrorState, [])


parseInput EndState cfg input = error ("No input line should ever be parsed in the end state, but we still got \""++input++"\"")


parseInput ErrorState cfg input = error ("No input line should ever be parsed in the error state, but we still got \""++input++"\"")


smCreate :: Cfg -> QuallifiedState
smCreate cfg = QuallifiedState StartState cfg


smStep (QuallifiedState s c) input =
    case parseInput s c input of
        (ErrorState, _) -> SmError "Failed"
        (EndState, _)   -> SmEnd
        (s', output)    -> SmOk (QuallifiedState s' c) output
