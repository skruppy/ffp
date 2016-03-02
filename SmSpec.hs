module SmSpec where

import Test.Hspec
import Sm
import Data.Array

cfg = Cfg {gameId = "GameId", player = Just 1}

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
            
            
    --describe "Parsing in player end state (+ ENDPLAYERS)" $ do





