-- "THE BEER-WARE LICENSE" (Revision 42):
-- Albatrouss and <skruppy@onmars.eu> wrote this software. As long as you retain
-- this notice you can do whatever you want with this stuff. If we meet some
-- day, and you think this stuff is worth it, you can buy me a beer in return.
--                                                     -- Albatrouss and Skruppy

module Main where

import Data.Array
import Sm.Internal
import Test.Hspec

testAi gameId gameData field time = ("test" , return ())
cfg = Cfg
    { gameId           = "GameId"
    , player           = Just 1
    , gameDataComplete = \_ _ -> return ()
    , preAi            = \_ _ _ _ -> return ()
    , ai               = testAi
    }

testPlayers = array (0, 1) [
    (0, Just PlayerItem {playerName = "Hans Peter" , isReady = True , itsMe = True }),
    (1, Just PlayerItem {playerName = "Horst"      , isReady = True , itsMe = False})]

gameData = GameData
    { serverMajor = 1
    , serverMinor = 23
    , gameType    = "Reversi"
    , gameName    = "Game name"
    , players     = array (0, 1)
        [(0, PlayerItem {playerName = "Hans Peter" , isReady = True , itsMe = True })
        ,(1, PlayerItem {playerName = "Horst"      , isReady = True , itsMe = False})]
    }

field = array ((1,1), (3,2)) [
    ((1,2), "2"), ((2,2), "3"), ((3,2), "5"),
    ((1,1), "7"), ((2,1),"11"), ((3,1),"13")]


-- Cut away IO from parser result, to be able to compare/test it
stateShouldBe (a, b, _) (a', b') = (a, b) `shouldBe` (a', b')


testStep (SmOk s o) (x:xs) = testStep s' xs
    where
        (s', _) = smStep s x

testStep result     []     = result

testStep result     _      = result


sm xs cfg = testStep (smCreate cfg) xs


main :: IO ()
main = hspec $ do
    describe "Parsing in start state (+ MNM Gameserver v1.? accepting connections)" $ do
        it "Valid version number (one-digit minor)" $ do
            (parseInput StartState cfg "+ MNM Gameserver v1.0 accepting connections")  `stateShouldBe` (VersionState 1  0, ["VERSION 1.42"])
        
        it "Valid version number (two-digit minor)" $ do
            (parseInput StartState cfg "+ MNM Gameserver v1.23 accepting connections") `stateShouldBe` (VersionState 1 23, ["VERSION 1.42"])
        
        it "Invalid version number (major != 1)" $ do
            (parseInput StartState cfg "+ MNM Gameserver v2.0 accepting connections")  `stateShouldBe` (ErrorState "Only protocol version 1 supported. Caussed by \"+ MNM Gameserver v2.0 accepting connections\"", [])
        
        it "Invalid input (zero-digit major)" $ do
            (parseInput StartState cfg "+ MNM Gameserver v.0 accepting connections")   `stateShouldBe` (ErrorState "Protocoll error: Expected server banner, but got \"+ MNM Gameserver v.0 accepting connections\"", [])
        
        it "Invalid input (zero-digit minor)" $ do
            (parseInput StartState cfg "+ MNM Gameserver v1. accepting connections")   `stateShouldBe` (ErrorState "Protocoll error: Expected server banner, but got \"+ MNM Gameserver v1. accepting connections\"", [])
        
        it "Invalid input (random)" $ do
            (parseInput StartState cfg "+ asd")                                        `stateShouldBe` (ErrorState "Protocoll error: Expected server banner, but got \"+ asd\"", [])
    
    
    describe "Parsing in version state (+ Client version accepted - please send Game-ID to join)" $ do
        it "Valid input" $ do
            parseInput (VersionState 1 23) cfg "+ Client version accepted - please send Game-ID to join" `stateShouldBe` (GameKindState 1 23, ["ID GameId"])
        
        it "Valid input (different game id)" $ do
            parseInput (VersionState 1 23) (cfg {gameId = "asd"}) "+ Client version accepted - please send Game-ID to join" `stateShouldBe` (GameKindState 1 23, ["ID asd"])
        
        it "Inalid input (random)" $ do
            parseInput (VersionState 1 23) cfg "+ asd" `stateShouldBe` (ErrorState "Protocoll error: Expected client version to be accepted, but got \"+ asd\"", [])
    
    
    describe "Parsing in game kind state (+ PLAYING ?)" $ do
        it "Valid input" $ do
            parseInput (GameKindState 1 23) cfg "+ PLAYING Reversi"  `stateShouldBe` (GameNameState 1 23 "Reversi", [])
        
        it "Valid input" $ do
            parseInput (GameKindState 1 23) cfg "+ PLAYING Checkers" `stateShouldBe` (GameNameState 1 23 "Checkers", [])
        
        it "Invalid input (missing game kind)" $ do
            parseInput (GameKindState 1 23) cfg  "+ PLAYING "        `stateShouldBe` (ErrorState "Protocoll error: Expected game kind, but got \"+ PLAYING \"", [])
        
        it "Invalid input (random)" $ do
            parseInput (GameKindState 1 23) cfg "+ asd"              `stateShouldBe` (ErrorState "Protocoll error: Expected game kind, but got \"+ asd\"", [])
    
    
    describe "Parsing in game kind state (+ ?)" $ do
        it "Valid input (one-digit player number)" $ do
            parseInput (GameNameState 1 23 "Reversi") (cfg {player = Just 1}) "+ Game name" `stateShouldBe` (PlayerNameState 1 23 "Reversi" "Game name", ["PLAYER 1"])
        
        it "Valid input (two-digit player number)" $ do
            parseInput (GameNameState 1 23 "Reversi") (cfg {player = Just 42}) "+ Game name" `stateShouldBe` (PlayerNameState 1 23 "Reversi" "Game name", ["PLAYER 42"])
        
        it "Valid input (no player number)" $ do
            parseInput (GameNameState 1 23 "Reversi") (cfg {player = Nothing}) "+ Game name" `stateShouldBe` (PlayerNameState 1 23 "Reversi" "Game name", ["PLAYER"])
        
        it "Invalid input (missing game name)" $ do
            parseInput (GameNameState 1 23 "Reversi") cfg  "+ " `stateShouldBe` (ErrorState "Protocoll error: Expected game name, but got \"+ \"", [])
        
        it "Invalid input (random)" $ do
            parseInput (GameNameState 1 23 "Reversi") cfg "asd" `stateShouldBe` (ErrorState "Protocoll error: Expected game name, but got \"asd\"", [])
    
    
    describe "Parsing in player start state (+ TOTAL ?)" $ do
        it "Valid input (one-digit player number)" $ do
            parseInput (PlayerStartState 1 23 "Reversi" "Game name" 0 (PlayerItem {playerName = "Hans Peter", isReady = True, itsMe = True})) cfg "+ TOTAL 2"
            `stateShouldBe`
            ( PlayerLineState 1 23 "Reversi" "Game name"
              ( array
                (0, 1)
                [ (0, Just $ PlayerItem {playerName = "Hans Peter", isReady = True, itsMe = True}),
                  (1, Nothing)
                ]
              )
              0
            , []
            )

        it "Valid input (two-digit player number)" $ do
            parseInput (PlayerStartState 1 23 "Reversi" "Game name" 0 (PlayerItem {playerName = "Hans Peter", isReady = True, itsMe = True})) cfg "+ TOTAL 12"
            `stateShouldBe`
            ( PlayerLineState
              1 23 "Reversi" "Game name"
              ( listArray
                (0, 11)
                ( (Just $ PlayerItem {playerName = "Hans Peter", isReady = True, itsMe = True})
                : (replicate 11 Nothing)
                )
              )
              10
            , []
            )

        it "Invalid input (zero players)" $ do
            parseInput (PlayerStartState 1 23 "Reversi" "Game name" 0 (PlayerItem {playerName = "Hans Peter", isReady = True, itsMe = True})) cfg "+ TOTAL 0" `stateShouldBe` (ErrorState "Protocoll error: Expected numer of oponents, but got \"+ TOTAL 0\"", [])

        it "Invalid input (no player number)" $ do
            parseInput (PlayerStartState 1 23 "Reversi" "Game name" 0 (PlayerItem {playerName = "Hans Peter", isReady = True, itsMe = True})) cfg "+ TOTAL " `stateShouldBe` (ErrorState "Protocoll error: Expected numer of oponents, but got \"+ TOTAL \"", [])
            
        it "Invalid input (random)" $ do
            parseInput (PlayerStartState 1 23 "Reversi" "Game name" 0 (PlayerItem {playerName = "Hans Peter", isReady = True, itsMe = True})) cfg "+ asd" `stateShouldBe` (ErrorState "Protocoll error: Expected numer of oponents, but got \"+ asd\"", [])
            
            
    describe "Parsing in player line state (+ ? ? ?)" $ do
        it "Valid input (lower bound)" $ do
            parseInput (
                PlayerLineState 1 23 "Reversi" "Game name"
                (array (0, 2) [
                    (0, Nothing),
                    (1, Nothing),
                    (2, Nothing)])
                1) cfg "+ 0 Hans Peter 1" `stateShouldBe` (
                    PlayerLineState 1 23 "Reversi" "Game name"
                    (array (0, 2) [
                        (0, Just $ PlayerItem {playerName = "Hans Peter", isReady = True, itsMe = False}),
                        (1, Nothing),
                        (2, Nothing)])
                    0, [])
                    
        it "Valid input (middle)" $ do
            parseInput (
                PlayerLineState 1 23 "Reversi" "Game name"
                (array (0, 2) [
                    (0, Nothing),
                    (1, Nothing),
                    (2, Nothing)])
                1) cfg "+ 1 Hans Peter 1" `stateShouldBe` (
                    PlayerLineState 1 23 "Reversi" "Game name"
                    (array (0, 2) [
                        (0, Nothing),
                        (1, Just $ PlayerItem {playerName = "Hans Peter", isReady = True, itsMe = False}),
                        (2, Nothing)])
                    0, [])
                    
        it "Valid input (upper bound)" $ do
            parseInput (
                PlayerLineState 1 23 "Reversi" "Game name"
                (array (0, 2) [
                    (0, Nothing),
                    (1, Nothing),
                    (2, Nothing)])
                1) cfg "+ 2 Hans Peter 1" `stateShouldBe` (
                    PlayerLineState 1 23 "Reversi" "Game name"
                    (array (0, 2) [
                        (0, Nothing),
                        (1, Nothing),
                        (2, Just $ PlayerItem {playerName = "Hans Peter", isReady = True, itsMe = False})])
                    0, [])
                    
        it "Valid input (not ready)" $ do
            parseInput (
                PlayerLineState 1 23 "Reversi" "Game name"
                (array (0, 2) [
                    (0, Nothing),
                    (1, Nothing),
                    (2, Nothing)])
                1) cfg "+ 1 Hans Peter 0" `stateShouldBe` (
                    PlayerLineState 1 23 "Reversi" "Game name"
                    (array (0, 2) [
                        (0, Nothing),
                        (1, Just $ PlayerItem {playerName = "Hans Peter", isReady = False, itsMe = False}),
                        (2, Nothing)])
                    0, [])
                    
        it "Valid input (got to end)" $ do
            parseInput (
                PlayerLineState 1 23 "Reversi" "Game name"
                (array (0, 1) [
                    (0, Nothing),
                    (1, Nothing)])
                0) cfg "+ 0 Hans Peter 1" `stateShouldBe` (
                    PlayerEndState 1 23 "Reversi" "Game name"
                    (array (0, 1) [
                        (0, Just $ PlayerItem {playerName = "Hans Peter", isReady = True, itsMe = False}),
                        (1, Nothing)]), [])
                    
        it "Invalid input (duplicate)" $ do
            parseInput (
                PlayerLineState 1 23 "Reversi" "Game name"
                (array (0, 2) [
                    (0, Nothing),
                    (1, Just $ PlayerItem {playerName = "Hans Peter", isReady = True, itsMe = False}),
                    (2, Nothing)])
                1) cfg "+ 1 Hans Peter 1" `stateShouldBe` (ErrorState "Player already defined. Caussed by \"+ 1 Hans Peter 1\"", [])
                    
        it "Invalid input (out of range)" $ do
            parseInput (
                PlayerLineState 1 23 "Reversi" "Game name"
                (array (0, 1) [
                    (0, Nothing),
                    (1, Nothing)])
                0) cfg "+ 3 Hans Peter 1" `stateShouldBe` (ErrorState "Player ID out of bounds. Caussed by \"+ 3 Hans Peter 1\"", [])
                    
        it "Invalid input (invalid ready state)" $ do
            parseInput (
                PlayerLineState 1 23 "Reversi" "Game name"
                (array (0, 1) [
                    (0, Nothing),
                    (1, Nothing)])
                0) cfg "+ 1 Hans Peter 2" `stateShouldBe` (ErrorState "Protocoll error: Expected oponent info, but got \"+ 1 Hans Peter 2\"", [])
                    
        it "Invalid input (missing ready state)" $ do
            parseInput (
                PlayerLineState 1 23 "Reversi" "Game name"
                (array (0, 1) [
                    (0, Nothing),
                    (1, Nothing)])
                0) cfg "+ 1 Hans Peter " `stateShouldBe` (ErrorState "Protocoll error: Expected oponent info, but got \"+ 1 Hans Peter \"", [])
                    
        it "Invalid input (missing player number)" $ do
            parseInput (
                PlayerLineState 1 23 "Reversi" "Game name"
                (array (0, 1) [
                    (0, Nothing),
                    (1, Nothing)])
                0) cfg "+  Hans Peter 1" `stateShouldBe` (ErrorState "Protocoll error: Expected oponent info, but got \"+  Hans Peter 1\"", [])
                    
        it "Invalid input (random)" $ do
            parseInput (
                PlayerLineState 1 23 "Reversi" "Game name"
                (array (0, 1) [
                    (0, Nothing),
                    (1, Nothing)])
                0) cfg "+ asd" `stateShouldBe` (ErrorState "Protocoll error: Expected oponent info, but got \"+ asd\"", [])
            
            
    describe "Parsing in player end state (+ ENDPLAYERS)" $ do
        it "Valid input" $ do
            parseInput (PlayerEndState 1 23 "Reversi" "Game name" testPlayers) cfg "+ ENDPLAYERS" `stateShouldBe` ((IdleState gameData), [])
            
        it "Invalid input" $ do
            parseInput (PlayerEndState 1 23 "Reversi" "Game name" testPlayers) cfg "+ asd" `stateShouldBe` (ErrorState "Protocoll error: Expected end of oponent list, but got \"+ asd\"", [])
            
            
    describe "Parsing in idle state (+ WAIT / + MOVE ? / + GAMEOVER ? ?)" $ do
        it "Valid input (wait)" $ do
            parseInput (IdleState gameData) cfg "+ WAIT" `stateShouldBe` ((IdleState gameData), ["OKWAIT"])
            
        it "Valid input (move)" $ do
            parseInput (IdleState gameData) cfg "+ MOVE 3000" `stateShouldBe` ((FieldStartState gameData (Move 3000)), [])
            
        it "Valid input (gameover draw)" $ do
            parseInput (IdleState gameData) cfg "+ GAMEOVER" `stateShouldBe` ((FieldStartState gameData Draw), [])
            
        it "Valid input (gameover winner)" $ do
            parseInput (IdleState gameData) cfg "+ GAMEOVER 1 Horst" `stateShouldBe` ((FieldStartState gameData (Winner 1)), [])
        
        it "Invalid input (gameover wrong name)" $ do
            parseInput (IdleState gameData) cfg "+ GAMEOVER 1 Hans Peter" `stateShouldBe` (ErrorState "The winner name is not part of the players list. Caussed by \"+ GAMEOVER 1 Hans Peter\"", [])
        
        it "Invalid input (gameover out of bounds)" $ do
            parseInput (IdleState gameData) cfg "+ GAMEOVER 42 Hans Peter" `stateShouldBe` (ErrorState "The winners player ID is out of bounds. Caussed by \"+ GAMEOVER 42 Hans Peter\"", [])
        
        it "Invalid input (move time 0)" $ do
            parseInput (IdleState gameData) cfg "+ MOVE 0" `stateShouldBe` (ErrorState "Protocoll error: Expected wait/gameover/move, but got \"+ MOVE 0\"", [])
            
        it "Invalid input (move missing time)" $ do
            parseInput (IdleState gameData) cfg "+ MOVE " `stateShouldBe` (ErrorState "Protocoll error: Expected wait/gameover/move, but got \"+ MOVE \"", [])
            
        it "Invalid input (random)" $ do
            parseInput (IdleState gameData) cfg "+ asd " `stateShouldBe` (ErrorState "Protocoll error: Expected wait/gameover/move, but got \"+ asd \"", [])
        
        it "Inalid input (gameover missing name)" $ do
            parseInput (IdleState gameData) cfg "+ GAMEOVER 1" `stateShouldBe` (ErrorState "Protocoll error: Expected wait/gameover/move, but got \"+ GAMEOVER 1\"", [])
        
        it "Inalid input (gameover missing number)" $ do
            parseInput (IdleState gameData) cfg "+ GAMEOVER Hans Peter" `stateShouldBe` (ErrorState "Protocoll error: Expected wait/gameover/move, but got \"+ GAMEOVER Hans Peter\"", [])
        
        it "Inalid input (random)" $ do
            parseInput (IdleState gameData) cfg "+ asd" `stateShouldBe` (ErrorState "Protocoll error: Expected wait/gameover/move, but got \"+ asd\"", [])
            
            
    describe "Parsing in field start state (+ FIELD ?,?)" $ do
        it "Valid input (one digit)" $ do
            parseInput (FieldStartState gameData (Move 42)) cfg "+ FIELD 2,3" `stateShouldBe` (
                (FieldLineState gameData (Move 42) 2 3 3 []), [])
                
        it "Valid input (two digit)" $ do
            parseInput (FieldStartState gameData (Move 42)) cfg "+ FIELD 22,33" `stateShouldBe` (
                (FieldLineState gameData (Move 42) 22 33 33 []), [])
                
        it "Invalid input (x=0)" $ do
            parseInput (FieldStartState gameData (Move 42)) cfg "+ FIELD 0,3" `stateShouldBe` (ErrorState "Protocoll error: Expected start of board, but got \"+ FIELD 0,3\"", [])
            
        it "Invalid input (y=0)" $ do
            parseInput (FieldStartState gameData (Move 42)) cfg "+ FIELD 2,0" `stateShouldBe` (ErrorState "Protocoll error: Expected start of board, but got \"+ FIELD 2,0\"", [])
            
        it "Invalid input (missing x)" $ do
            parseInput (FieldStartState gameData (Move 42)) cfg "+ FIELD ,3" `stateShouldBe` (ErrorState "Protocoll error: Expected start of board, but got \"+ FIELD ,3\"", [])
            
        it "Invalid input (missing y)" $ do
            parseInput (FieldStartState gameData (Move 42)) cfg "+ FIELD 2," `stateShouldBe` (ErrorState "Protocoll error: Expected start of board, but got \"+ FIELD 2,\"", [])
            
        it "Invalid input (random)" $ do
            parseInput (FieldStartState gameData (Move 42)) cfg "+ asd" `stateShouldBe` (ErrorState "Protocoll error: Expected start of board, but got \"+ asd\"", [])


    describe "Parsing in field end state (+ ? ? ? ? ? ...)" $ do
        it "Valid input (first line)" $ do
            parseInput (FieldLineState gameData (Move 3000) 3 2 2 []) cfg "+ 2 2 3 5" `stateShouldBe` (
                (FieldLineState gameData (Move 3000) 3 2 1 [["2","3","5"]]), [])
        
        it "Valid input (last line)" $ do
            parseInput (FieldLineState gameData (Move 3000) 3 2 1 [["2","3","5"]]) cfg "+ 1 7 11 13" `stateShouldBe` (
                (FieldEndState gameData (Move 3000) 3 2 ([["7","11","13"],["2","3","5"]]) ), [])
        
        it "Invalid input (wrong line number)" $ do
            parseInput (FieldLineState gameData (Move 3000) 3 2 2 []) cfg "+ 1 7 11 13" `stateShouldBe` (ErrorState "Unexpected row number. Caussed by \"+ 1 7 11 13\"", [])
        
        it "Invalid input (to long)" $ do
            parseInput (FieldLineState gameData (Move 3000) 3 2 2 []) cfg "+ 2 7 11 13 23" `stateShouldBe` (ErrorState "Row has invalid number of elements. Caussed by \"+ 2 7 11 13 23\"", [])
        
        it "Invalid input (to short)" $ do
            parseInput (FieldLineState gameData (Move 3000) 3 2 2 []) cfg "+ 2 7 11" `stateShouldBe` (ErrorState "Row has invalid number of elements. Caussed by \"+ 2 7 11\"", [])
        
        it "Invalid input (random)" $ do
            parseInput (FieldLineState gameData (Move 3000) 3 2 2 []) cfg "+ asd" `stateShouldBe` (ErrorState "Protocoll error: Expected board row, but got \"+ asd\"", [])
            
            
    describe "Parsing in field end state (+ ENDFIELD)" $ do
        it "Valid input (AI)" $ do
            parseInput (
                    FieldEndState gameData (Move 3000) 3 2
                    ([["7","11","13"],["2","3","5"]]) )
                cfg "+ ENDFIELD" `stateShouldBe` ((ThinkingState gameData 3000 field), ["THINKING"])
        
        it "Valid input (quit)" $ do
            parseInput (
                    FieldEndState gameData Draw 3 2
                    ([["7","11","13"],["2","3","5"]]) )
                cfg "+ ENDFIELD" `stateShouldBe` (QuitState gameData Nothing field, [])
        
        it "Valid input (quit)" $ do
            parseInput (
                    FieldEndState gameData (Winner 0) 3 2
                    ([["7","11","13"],["2","3","5"]]) )
                cfg "+ ENDFIELD" `stateShouldBe` (QuitState gameData (Just 0) field, [])
            
        it "Invalid input" $ do
            parseInput (FieldEndState gameData (Move 3000) 3 2 [["7","11","13"],["2","3","5"]]) cfg "+ asd" `stateShouldBe` (ErrorState "Protocoll error: Expected end of board, but got \"+ asd\"", [])

            
    describe "Parsing in player end state (+ OKTHINK)" $ do
        it "Valid input" $ do
            parseInput (ThinkingState gameData 3000 field) cfg "+ OKTHINK" `stateShouldBe` ((MoveState gameData), ["PLAY test"])
            
        it "Invalid input" $ do
            parseInput (ThinkingState gameData 3000 field) cfg "+ asd" `stateShouldBe` (ErrorState "Protocoll error: Expected OKTHINK, but got \"+ asd\"", [])
            
            
    describe "Parsing in player end state (+ MOVEOK)" $ do
        it "Valid input" $ do
            parseInput (MoveState gameData) cfg "+ MOVEOK" `stateShouldBe` ((IdleState gameData), [])
            
        it "Invalid input" $ do
            parseInput (MoveState gameData) cfg "+ asd" `stateShouldBe` (ErrorState "Protocoll error: Expected acceptance of our move, but got \"+ asd\"", [])
            
            
    describe "Parsing in player end state (+ QUIT)" $ do
        it "Valid input" $ do
            parseInput (QuitState gameData (Just 0) field) cfg "+ QUIT" `stateShouldBe` (EndState gameData (Just 0) field, [])
            
        it "Invalid input" $ do
            parseInput (QuitState gameData (Just 0) field) cfg "+ asd" `stateShouldBe` (ErrorState "Protocoll error: Expected quit, but got \"+ asd\"", [])
            
            


    describe "Full test" $ do
        it "Valid input (big scenario)" $ do
            sm [
                "+ MNM Gameserver v1.23 accepting connections",-- VERSION 1.0
                "+ Client version accepted - please send Game-ID to join", -- ID ...
                "+ PLAYING Reversi",
                "+ Game name", -- PLAYER ...
                "+ YOU 0 Hans Peter",
                "+ TOTAL 2",
                "+ 1 Horst 1",
                "+ ENDPLAYERS",
                "+ WAIT", -- OKWAIT
                "+ WAIT", -- OKWAIT
                "+ MOVE 3000",
                "+ FIELD 3,2",
                "+ 2 2 3 5",
                "+ 1 7 11 13",
                "+ ENDFIELD", -- THINKING
                "+ OKTHINK", -- PLAY ...
                "+ MOVEOK",
                "+ WAIT", -- OKWAIT
                "+ WAIT", -- OKWAIT
                "+ GAMEOVER 0 Hans Peter",
                "+ FIELD 3,2",
                "+ 2 2 3 5",
                "+ 1 7 11 13",
                "+ ENDFIELD",
                "+ QUIT"] cfg `shouldBe` SmEnd gameData (Just 1) field
                
        it "Valid input (instant gameover)" $ do
            sm [
                "+ MNM Gameserver v1.23 accepting connections",-- VERSION 1.0
                "+ Client version accepted - please send Game-ID to join", -- ID ...
                "+ PLAYING Reversi",
                "+ Game name", -- PLAYER ...
                "+ YOU 0 Hans Peter",
                "+ TOTAL 2",
                "+ 1 Horst 1",
                "+ ENDPLAYERS",
                "+ GAMEOVER 0 Hans Peter",
                "+ FIELD 3,2",
                "+ 2 2 3 5",
                "+ 1 7 11 13",
                "+ ENDFIELD",
                "+ QUIT"] cfg `shouldBe` SmEnd gameData (Just 1) field
                
        it "Invalid input (missing last field)" $ do
            sm [
                "+ MNM Gameserver v1.23 accepting connections",-- VERSION 1.0
                "+ Client version accepted - please send Game-ID to join", -- ID ...
                "+ PLAYING Reversi",
                "+ Game name", -- PLAYER ...
                "+ YOU 0 Hans Peter",
                "+ TOTAL 2",
                "+ 1 Horst 1",
                "+ ENDPLAYERS",
                "+ GAMEOVER 0 Hans Peter",
                "+ FIELD 3,2",
                "+ 2 2 3 5",
                "+ ENDFIELD",
                "+ QUIT"] cfg `shouldBe` SmError "Protocoll error: Expected board row, but got \"+ ENDFIELD\""
                
        it "Invalid input (missing first field)" $ do
            sm [
                "+ MNM Gameserver v1.23 accepting connections",-- VERSION 1.0
                "+ Client version accepted - please send Game-ID to join", -- ID ...
                "+ PLAYING Reversi",
                "+ Game name", -- PLAYER ...
                "+ YOU 0 Hans Peter",
                "+ TOTAL 2",
                "+ 1 Horst 1",
                "+ ENDPLAYERS",
                "+ GAMEOVER 0 Hans Peter",
                "+ FIELD 3,2",
                "+ 1 7 11 13",
                "+ ENDFIELD",
                "+ QUIT"] cfg `shouldBe` SmError "Unexpected row number. Caussed by \"+ 1 7 11 13\""
                
        it "Invalid input (missing other players)" $ do
            sm [
                "+ MNM Gameserver v1.23 accepting connections",-- VERSION 1.0
                "+ Client version accepted - please send Game-ID to join", -- ID ...
                "+ PLAYING Reversi",
                "+ Game name", -- PLAYER ...
                "+ YOU 0 Hans Peter",
                "+ TOTAL 2",
                "+ ENDPLAYERS",
                "+ GAMEOVER 0 Hans Peter",
                "+ FIELD 3,2",
                "+ 2 2 3 5",
                "+ 1 7 11 13",
                "+ ENDFIELD",
                "+ QUIT"] cfg `shouldBe` SmError "Protocoll error: Expected oponent info, but got \"+ ENDPLAYERS\""
                
        it "Invalid input (duplicated players)" $ do
            sm [
                "+ MNM Gameserver v1.23 accepting connections",-- VERSION 1.0
                "+ Client version accepted - please send Game-ID to join", -- ID ...
                "+ PLAYING Reversi",
                "+ Game name", -- PLAYER ...
                "+ YOU 0 Hans Peter",
                "+ TOTAL 3",
                "+ 1 Horst 1",
                "+ 1 Horst 1",
                "+ ENDPLAYERS",
                "+ GAMEOVER 0 Hans Peter",
                "+ FIELD 3,2",
                "+ 2 2 3 5",
                "+ 1 7 11 13",
                "+ ENDFIELD",
                "+ QUIT"] cfg `shouldBe` SmError "Player already defined. Caussed by \"+ 1 Horst 1\""
                
        it "Invalid input (to many players)" $ do
            sm [
                "+ MNM Gameserver v1.23 accepting connections",-- VERSION 1.0
                "+ Client version accepted - please send Game-ID to join", -- ID ...
                "+ PLAYING Reversi",
                "+ Game name", -- PLAYER ...
                "+ YOU 0 Hans Peter",
                "+ TOTAL 2",
                "+ 1 Horst 1",
                "+ 2 Player 1 1",
                "+ ENDPLAYERS",
                "+ GAMEOVER 0 Hans Peter",
                "+ FIELD 3,2",
                "+ 2 2 3 5",
                "+ 1 7 11 13",
                "+ ENDFIELD",
                "+ QUIT"] cfg `shouldBe` SmError "Protocoll error: Expected end of oponent list, but got \"+ 2 Player 1 1\""
