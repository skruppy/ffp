module SmSpec where

import Test.Hspec
import Sm
import Data.Array

cfg = Cfg {gameId = "GameId", player = Just 1}

players = array (0, 1) [
    (0, PlayerItem {playerName = Just "Hans Peter", isReady = True}),
    (1, PlayerItem {playerName = Nothing, isReady = True})]

field = array ((1,1), (3,2)) [
    ((1,2), "2"), ((2,2), "3"), ((3,2), "5"),
    ((1,1), "7"), ((2,1),"11"), ((3,1),"13")]

main :: IO ()
main = hspec $ do
    describe "Parsing in start state (+ MNM Gameserver v1.? accepting connections)" $ do
        it "Valid version number (one-digit minor)" $ do
            (parseInput StartState cfg "+ MNM Gameserver v1.0 accepting connections") `shouldBe` (VersionState, ["VERSION 1.42"])
        
        it "Valid version number (two-digit minor)" $ do
            (parseInput StartState cfg "+ MNM Gameserver v1.23 accepting connections") `shouldBe` (VersionState, ["VERSION 1.42"])
        
        it "Invalid version number (major != 1)" $ do
            (parseInput StartState cfg "+ MNM Gameserver v2.0 accepting connections") `shouldBe` (ErrorState, [])
        
        it "Invalid input (random)" $ do
            (parseInput StartState cfg "+ asd") `shouldBe` (ErrorState, [])
        
        it "Invalid input (zero-digit major)" $ do
            (parseInput StartState cfg "+ MNM Gameserver v.0 accepting connections") `shouldBe` (ErrorState, [])
        
        it "Invalid input (zero-digit minor)" $ do
            (parseInput StartState cfg "+ MNM Gameserver v1. accepting connections") `shouldBe` (ErrorState, [])
    
    
    describe "Parsing in version state (+ Client version accepted - please send Game-ID to join)" $ do
        it "Valid input" $ do
            parseInput VersionState cfg "+ Client version accepted - please send Game-ID to join" `shouldBe` (GameKindState, ["ID GameId"])
        
        it "Valid input (different game id)" $ do
            parseInput VersionState (Cfg {gameId = "asd", player = Just 1}) "+ Client version accepted - please send Game-ID to join" `shouldBe` (GameKindState, ["ID asd"])
        
        it "Inalid input (random)" $ do
            parseInput VersionState cfg "+ asd" `shouldBe` (ErrorState, [])
    
    
    describe "Parsing in game kind state (+ PLAYING ?)" $ do
        it "Valid input" $ do -- Different games
            parseInput GameKindState cfg "+ PLAYING Reversi" `shouldBe` (GameNameState, [])
        
        it "Valid input" $ do -- Different games
            parseInput GameKindState cfg "+ PLAYING Checkers" `shouldBe` (GameNameState, [])
        
        it "Invalid input (missing game kind)" $ do
            parseInput GameKindState cfg  "+ PLAYING " `shouldBe` (ErrorState, [])
        
        it "Invalid input (random)" $ do
            parseInput GameKindState cfg "+ asd" `shouldBe` (ErrorState, [])
    
    
    describe "Parsing in game kind state (+ ?)" $ do
        it "Valid input (one-digit player number)" $ do
            parseInput GameNameState (Cfg {gameId = "GameId", player = Just 1}) "+ Game name" `shouldBe` (PlayerStartState, ["PLAYER 1"])
        
        it "Valid input (two-digit player number)" $ do
            parseInput GameNameState (Cfg {gameId = "GameId", player = Just 42}) "+ Game name" `shouldBe` (PlayerStartState, ["PLAYER 42"])
        
        it "Valid input (no player number)" $ do
            parseInput GameNameState (Cfg {gameId = "GameId", player = Nothing}) "+ Game name" `shouldBe` (PlayerStartState, ["PLAYER"])
        
        it "Invalid input (missing game name)" $ do
            parseInput GameNameState cfg  "+ " `shouldBe` (ErrorState, [])
        
        it "Invalid input (random)" $ do
            parseInput GameNameState cfg "asd" `shouldBe` (ErrorState, [])
    
    
    describe "Parsing in player start state (+ TOTAL ?)" $ do
        it "Valid input (one-digit player number)" $ do
            parseInput PlayerStartState cfg "+ TOTAL 2" `shouldBe` (
                PlayerLineState
                (array (0, 1)  [
                    (0, PlayerItem {playerName = Nothing, isReady = True}),
                    (1, PlayerItem {playerName = Nothing, isReady = True})])
                0, [])

        it "Valid input (two-digit player number)" $ do
            parseInput PlayerStartState cfg "+ TOTAL 12" `shouldBe` (
                PlayerLineState
                (listArray (0, 11)  (replicate 12 PlayerItem {playerName = Nothing, isReady = True})) 10, [])

        it "Invalid input (zero players)" $ do
            parseInput PlayerStartState cfg "+ TOTAL 0" `shouldBe` (ErrorState, [])

        it "Invalid input (no player number)" $ do
            parseInput PlayerStartState cfg "+ TOTAL " `shouldBe` (ErrorState, [])
            
        it "Invalid input (random)" $ do
            parseInput PlayerStartState cfg "+ asd" `shouldBe` (ErrorState, [])
            
            
    describe "Parsing in player line state (+ ? ? ?)" $ do
        it "Valid input (lower bound)" $ do
            parseInput (
                PlayerLineState
                (array (0, 2) [
                    (0, PlayerItem {playerName = Nothing, isReady = True}),
                    (1, PlayerItem {playerName = Nothing, isReady = True}),
                    (2, PlayerItem {playerName = Nothing, isReady = True})])
                1) cfg "+ 0 Hans Peter 1" `shouldBe` (
                    PlayerLineState
                    (array (0, 2) [
                        (0, PlayerItem {playerName = Just "Hans Peter", isReady = True}),
                        (1, PlayerItem {playerName = Nothing, isReady = True}),
                        (2, PlayerItem {playerName = Nothing, isReady = True})])
                    0, [])
                    
        it "Valid input (middle)" $ do
            parseInput (
                PlayerLineState
                (array (0, 2) [
                    (0, PlayerItem {playerName = Nothing, isReady = True}),
                    (1, PlayerItem {playerName = Nothing, isReady = True}),
                    (2, PlayerItem {playerName = Nothing, isReady = True})])
                1) cfg "+ 1 Hans Peter 1" `shouldBe` (
                    PlayerLineState
                    (array (0, 2) [
                        (0, PlayerItem {playerName = Nothing, isReady = True}),
                        (1, PlayerItem {playerName = Just "Hans Peter", isReady = True}),
                        (2, PlayerItem {playerName = Nothing, isReady = True})])
                    0, [])
                    
        it "Valid input (upper bound)" $ do
            parseInput (
                PlayerLineState
                (array (0, 2) [
                    (0, PlayerItem {playerName = Nothing, isReady = True}),
                    (1, PlayerItem {playerName = Nothing, isReady = True}),
                    (2, PlayerItem {playerName = Nothing, isReady = True})])
                1) cfg "+ 2 Hans Peter 1" `shouldBe` (
                    PlayerLineState
                    (array (0, 2) [
                        (0, PlayerItem {playerName = Nothing, isReady = True}),
                        (1, PlayerItem {playerName = Nothing, isReady = True}),
                        (2, PlayerItem {playerName = Just "Hans Peter", isReady = True})])
                    0, [])
                    
        it "Valid input (not ready)" $ do
            parseInput (
                PlayerLineState
                (array (0, 2) [
                    (0, PlayerItem {playerName = Nothing, isReady = True}),
                    (1, PlayerItem {playerName = Nothing, isReady = True}),
                    (2, PlayerItem {playerName = Nothing, isReady = True})])
                1) cfg "+ 1 Hans Peter 0" `shouldBe` (
                    PlayerLineState
                    (array (0, 2) [
                        (0, PlayerItem {playerName = Nothing, isReady = True}),
                        (1, PlayerItem {playerName = Just "Hans Peter", isReady = False}),
                        (2, PlayerItem {playerName = Nothing, isReady = True})])
                    0, [])
                    
        it "Valid input (got to end)" $ do
            parseInput (
                PlayerLineState
                (array (0, 1) [
                    (0, PlayerItem {playerName = Nothing, isReady = True}),
                    (1, PlayerItem {playerName = Nothing, isReady = True})])
                0) cfg "+ 0 Hans Peter 1" `shouldBe` (
                    PlayerEndState
                    (array (0, 1) [
                        (0, PlayerItem {playerName = Just "Hans Peter", isReady = True}),
                        (1, PlayerItem {playerName = Nothing, isReady = True})]), [])
                    
        it "Invalid input (duplicate)" $ do
            parseInput (
                PlayerLineState
                (array (0, 2) [
                    (0, PlayerItem {playerName = Nothing, isReady = True}),
                    (1, PlayerItem {playerName = Just "Hans Peter", isReady = True}),
                    (2, PlayerItem {playerName = Nothing, isReady = True})])
                1) cfg "+ 1 Hans Peter 1" `shouldBe` (ErrorState, [])
                    
        it "Invalid input (out of range)" $ do
            parseInput (
                PlayerLineState
                (array (0, 1) [
                    (0, PlayerItem {playerName = Nothing, isReady = True}),
                    (1, PlayerItem {playerName = Nothing, isReady = True})])
                0) cfg "+ 3 Hans Peter 1" `shouldBe` (ErrorState, [])
                    
        it "Invalid input (invalid ready state)" $ do
            parseInput (
                PlayerLineState
                (array (0, 1) [
                    (0, PlayerItem {playerName = Nothing, isReady = True}),
                    (1, PlayerItem {playerName = Nothing, isReady = True})])
                0) cfg "+ 1 Hans Peter 2" `shouldBe` (ErrorState, [])
                    
        it "Invalid input (missing ready state)" $ do
            parseInput (
                PlayerLineState
                (array (0, 1) [
                    (0, PlayerItem {playerName = Nothing, isReady = True}),
                    (1, PlayerItem {playerName = Nothing, isReady = True})])
                0) cfg "+ 1 Hans Peter " `shouldBe` (ErrorState, [])
                    
        it "Invalid input (missing player number)" $ do
            parseInput (
                PlayerLineState
                (array (0, 1) [
                    (0, PlayerItem {playerName = Nothing, isReady = True}),
                    (1, PlayerItem {playerName = Nothing, isReady = True})])
                0) cfg "+  Hans Peter 1" `shouldBe` (ErrorState, [])
                    
        it "Invalid input (random)" $ do
            parseInput (
                PlayerLineState
                (array (0, 1) [
                    (0, PlayerItem {playerName = Nothing, isReady = True}),
                    (1, PlayerItem {playerName = Nothing, isReady = True})])
                0) cfg "+ asd" `shouldBe` (ErrorState, [])
            
            
    describe "Parsing in player end state (+ ENDPLAYERS)" $ do
        it "Valid input" $ do
            parseInput (PlayerEndState players) cfg "+ ENDPLAYERS" `shouldBe` ((IdleState players), [])
            
        it "Invalid input" $ do
            parseInput (PlayerEndState players) cfg "+ asd" `shouldBe` (ErrorState, [])
            
            
    describe "Parsing in idle state (+ WAIT / + MOVE ? / + GAMEOVER ? ?)" $ do
        it "Valid input (wait)" $ do
            parseInput (IdleState players) cfg "+ WAIT" `shouldBe` ((IdleState players), ["OKWAIT"])
            
        it "Valid input (move)" $ do
            parseInput (IdleState players) cfg "+ MOVE 3000" `shouldBe` ((FieldStartState players (Just 3000)), [])
            
        it "Valid input (gameover)" $ do
            parseInput (IdleState players) cfg "+ GAMEOVER 1 Hans Peter" `shouldBe` ((FieldStartState players Nothing), [])
        
        it "Invalid input (move time 0)" $ do
            parseInput (IdleState players) cfg "+ MOVE 0" `shouldBe` (ErrorState, [])
            
        it "Invalid input (move missing time)" $ do
            parseInput (IdleState players) cfg "+ MOVE " `shouldBe` (ErrorState, [])
            
        it "Invalid input (random)" $ do
            parseInput (IdleState players) cfg "+ asd " `shouldBe` (ErrorState, [])
        
        it "Inalid input (gameover missing name)" $ do
            parseInput (IdleState players) cfg "+ GAMEOVER 1" `shouldBe` (ErrorState, [])
        
        it "Inalid input (gameover missing number)" $ do
            parseInput (IdleState players) cfg "+ GAMEOVER Hans Peter" `shouldBe` (ErrorState, [])
        
        it "Inalid input (random)" $ do
            parseInput (IdleState players) cfg "+ asd" `shouldBe` (ErrorState, [])
            
            
    describe "Parsing in field start state (+ FIELD ?,?)" $ do
        it "Valid input (one digit)" $ do
            parseInput (FieldStartState players (Just 42)) cfg "+ FIELD 2,3" `shouldBe` (
                (FieldLineState players (Just 42) 2 3 3 []), [])
                
        it "Valid input (two digit)" $ do
            parseInput (FieldStartState players (Just 42)) cfg "+ FIELD 22,33" `shouldBe` (
                (FieldLineState players (Just 42) 22 33 33 []), [])
                
        it "Invalid input (x=0)" $ do
            parseInput (FieldStartState players (Just 42)) cfg "+ FIELD 0,3" `shouldBe` (ErrorState, [])
            
        it "Invalid input (y=0)" $ do
            parseInput (FieldStartState players (Just 42)) cfg "+ FIELD 2,0" `shouldBe` (ErrorState, [])
            
        it "Invalid input (missing x)" $ do
            parseInput (FieldStartState players (Just 42)) cfg "+ FIELD ,3" `shouldBe` (ErrorState, [])
            
        it "Invalid input (missing y)" $ do
            parseInput (FieldStartState players (Just 42)) cfg "+ FIELD 2," `shouldBe` (ErrorState, [])
            
        it "Invalid input (random)" $ do
            parseInput (FieldStartState players (Just 42)) cfg "+ asd" `shouldBe` (ErrorState, [])


    describe "Parsing in field end state (+ ? ? ? ? ? ...)" $ do
        it "Valid input (first line)" $ do
            parseInput (FieldLineState players (Just 3000) 3 2 2 []) cfg "+ 2 2 3 5" `shouldBe` (
                (FieldLineState players (Just 3000) 3 2 1 [["2","3","5"]]), [])
        
        it "Valid input (last line)" $ do
            parseInput (FieldLineState players (Just 3000) 3 2 1 [["2","3","5"]]) cfg "+ 1 7 11 13" `shouldBe` (
                (FieldEndState players (Just 3000) 3 2 ([["7","11","13"],["2","3","5"]]) ), [])
        
        it "Invalid input (wrong line number)" $ do
            parseInput (FieldLineState players (Just 3000) 3 2 2 []) cfg "+ 1 7 11 13" `shouldBe` (ErrorState, [])
        
        it "Invalid input (to long)" $ do
            parseInput (FieldLineState players (Just 3000) 3 2 2 []) cfg "+ 2 7 11 13 23" `shouldBe` (ErrorState, [])
        
        it "Invalid input (to short)" $ do
            parseInput (FieldLineState players (Just 3000) 3 2 2 []) cfg "+ 2 7 11" `shouldBe` (ErrorState, [])
        
        it "Invalid input (random)" $ do
            parseInput (FieldLineState players (Just 3000) 3 2 2 []) cfg "+ asd" `shouldBe` (ErrorState, [])
            
            
    describe "Parsing in field end state (+ ENDFIELD)" $ do
        it "Valid input (AI)" $ do
            parseInput (
                    FieldEndState players (Just 3000) 3 2
                    ([["7","11","13"],["2","3","5"]]) )
                cfg "+ ENDFIELD" `shouldBe` ((ThinkingState players 3000 field), ["THINKING"])
        
        it "Valid input (quit)" $ do
            parseInput (
                    FieldEndState players Nothing 3 2
                    ([["7","11","13"],["2","3","5"]]) )
                cfg "+ ENDFIELD" `shouldBe` (QuitState, [])
            
        it "Invalid input" $ do
            parseInput (FieldEndState players (Just 3000) 3 2 [["7","11","13"],["2","3","5"]]) cfg "+ asd" `shouldBe` (ErrorState, [])

            
    describe "Parsing in player end state (+ OKTHINK)" $ do
        it "Valid input" $ do
            parseInput (ThinkingState players 3000 field) cfg "+ OKTHINK" `shouldBe` ((MoveState players), ["PLAY test"])
            
        it "Invalid input" $ do
            parseInput (ThinkingState players 3000 field) cfg "+ asd" `shouldBe` (ErrorState, [])
            
            
    describe "Parsing in player end state (+ MOVEOK)" $ do
        it "Valid input" $ do
            parseInput (MoveState players) cfg "+ MOVEOK" `shouldBe` ((IdleState players), [])
            
        it "Invalid input" $ do
            parseInput (MoveState players) cfg "+ asd" `shouldBe` (ErrorState, [])
            
            
    describe "Parsing in player end state (+ QUIT)" $ do
        it "Valid input" $ do
            parseInput QuitState cfg "+ QUIT" `shouldBe` (EndState, [])
            
        it "Invalid input" $ do
            parseInput QuitState cfg "+ asd" `shouldBe` (ErrorState, [])
            
            
    describe "Full test" $ do
        it "Valid input (big scenario)" $ do
            sm [
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
                "+ QUIT"] cfg `shouldBe` True
                
        it "Valid input (instant gameover)" $ do
            sm [
                "+ MNM Gameserver v1.0 accepting connections",-- VERSION 1.0
                "+ Client version accepted - please send Game-ID to join", -- ID ...
                "+ PLAYING Reversi",
                "+ The name of the game", -- PLAYER ...
                "+ TOTAL 2",
                "+ 1 Player 1 1",
                "+ ENDPLAYERS",
                "+ GAMEOVER 0 Your Name",
                "+ FIELD 4,4",
                "+ 4 a b c d",
                "+ 3 e f g h",
                "+ 2 i j k l",
                "+ 1 m n o p",
                "+ ENDFIELD",
                "+ QUIT"] cfg `shouldBe` True
                
        it "Valid input (missing last field)" $ do
            sm [
                "+ MNM Gameserver v1.0 accepting connections",-- VERSION 1.0
                "+ Client version accepted - please send Game-ID to join", -- ID ...
                "+ PLAYING Reversi",
                "+ The name of the game", -- PLAYER ...
                "+ TOTAL 2",
                "+ 1 Player 1 1",
                "+ ENDPLAYERS",
                "+ GAMEOVER 0 Your Name",
                "+ FIELD 4,4",
                "+ 4 a b c d",
                "+ 3 e f g h",
                "+ 2 i j k l",
                "+ ENDFIELD",
                "+ QUIT"] cfg `shouldBe` False
                
        it "Valid input (missing first field)" $ do
            sm [
                "+ MNM Gameserver v1.0 accepting connections",-- VERSION 1.0
                "+ Client version accepted - please send Game-ID to join", -- ID ...
                "+ PLAYING Reversi",
                "+ The name of the game", -- PLAYER ...
                "+ TOTAL 2",
                "+ 1 Player 1 1",
                "+ ENDPLAYERS",
                "+ GAMEOVER 0 Your Name",
                "+ FIELD 4,4",
                "+ 3 e f g h",
                "+ 2 i j k l",
                "+ 1 m n o p",
                "+ ENDFIELD",
                "+ QUIT"] cfg `shouldBe` False
                
        it "Valid input (missing other players)" $ do
            sm [
                "+ MNM Gameserver v1.0 accepting connections",-- VERSION 1.0
                "+ Client version accepted - please send Game-ID to join", -- ID ...
                "+ PLAYING Reversi",
                "+ The name of the game", -- PLAYER ...
                "+ TOTAL 2",
                "+ ENDPLAYERS",
                "+ GAMEOVER 0 Your Name",
                "+ FIELD 4,4",
                "+ 4 a b c d",
                "+ 3 e f g h",
                "+ 2 i j k l",
                "+ 1 m n o p",
                "+ ENDFIELD",
                "+ QUIT"] cfg `shouldBe` False
                
        it "Valid input (duplicated players)" $ do
            sm [
                "+ MNM Gameserver v1.0 accepting connections",-- VERSION 1.0
                "+ Client version accepted - please send Game-ID to join", -- ID ...
                "+ PLAYING Reversi",
                "+ The name of the game", -- PLAYER ...
                "+ TOTAL 3",
                "+ 1 Player 1 1",
                "+ 1 Player 1 1",
                "+ ENDPLAYERS",
                "+ GAMEOVER 0 Your Name",
                "+ FIELD 4,4",
                "+ 4 a b c d",
                "+ 3 e f g h",
                "+ 2 i j k l",
                "+ 1 m n o p",
                "+ ENDFIELD",
                "+ QUIT"] cfg `shouldBe` False
                
        it "Valid input (to many players)" $ do
            sm [
                "+ MNM Gameserver v1.0 accepting connections",-- VERSION 1.0
                "+ Client version accepted - please send Game-ID to join", -- ID ...
                "+ PLAYING Reversi",
                "+ The name of the game", -- PLAYER ...
                "+ TOTAL 2",
                "+ 1 Player 1 1",
                "+ 2 Player 1 1",
                "+ ENDPLAYERS",
                "+ GAMEOVER 0 Your Name",
                "+ FIELD 4,4",
                "+ 4 a b c d",
                "+ 3 e f g h",
                "+ 2 i j k l",
                "+ 1 m n o p",
                "+ ENDFIELD",
                "+ QUIT"] cfg `shouldBe` False
                
        it "Valid input (not complete)" $ do
            sm [
                "+ MNM Gameserver v1.0 accepting connections",-- VERSION 1.0
                "+ Client version accepted - please send Game-ID to join", -- ID ...
                "+ PLAYING Reversi",
                "+ The name of the game", -- PLAYER ...
                "+ TOTAL 2",
                "+ 1 Player 1 1",
                "+ ENDPLAYERS",
                "+ GAMEOVER 0 Your Name",
                "+ FIELD 4,4",
                "+ 4 a b c d",
                "+ 3 e f g h",
                "+ 2 i j k l",
                "+ 1 m n o p",
                "+ ENDFIELD"] cfg `shouldBe` False


