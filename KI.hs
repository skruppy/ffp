-- "THE BEER-WARE LICENSE" (Revision 42):
-- Albatrouss and <skruppy@onmars.eu> wrote this software. As long as you retain
-- this notice you can do whatever you want with this stuff. If we meet some
-- day, and you think this stuff is worth it, you can buy me a beer in return.
--                                                     -- Albatrouss and Skruppy

module KI where

import Data.Tree.Game_tree.Negascout as NS
import Data.Tree.Game_tree.Game_tree 
import Data.Array
import Sm
import Control.Concurrent.MVar
import System.IO.Unsafe

alphabet = ['A'..'Z'];

directions = [
    (-1,-1) , (-1, 0) , (-1, 1),
    ( 0,-1) ,           ( 0, 1),
    ( 1,-1) , ( 1, 0) , ( 1, 1)]

searchdepth = -200

data RNode = RNode
    { gamefield   :: (Array (Int, Int) (Int, String))
    ,playerColour :: String -- playerNumber 0 => B; 1 => W
    , playerTurn  :: String --whose turn it is --> B/W
    , lastMove    :: Maybe (Int,Int)
    } deriving (Eq, Show)


getPlayerColourFromGameData :: GameData -> String
getPlayerColourFromGameData (GameData _ _ _ _ players) = itsMe $ firstItem
    where
        itsMe (PlayerItem _ _ True ) = "B"
        itsMe (PlayerItem _ _ False) = "W"
        firstItem = players ! 0


getNextMove :: (MVar (Array (Int, Int) String)) -> (MVar GameData)-> (Array (Int, Int) String) -> GameData  -> String
getNextMove mVarField mVarGameData field gameData =
    if ( t == True)
        then getMoveFromRNode $ getNextRNode a searchdepth
        else getMoveFromRNode $ getNextRNode a searchdepth
    where
        a  = (RNode (createWeightedArray pC field) pC pC Nothing)
        pC = getPlayerColourFromGameData gameData
        t  = unsafePerformIO $ ((tryPutMVar mVarField field)>>(tryPutMVar mVarGameData gameData))


getNextRNode :: RNode -> Int -> RNode
getNextRNode a i = (fst $ negamax a i) !! 1


getMoveFromRNode:: RNode -> String
getMoveFromRNode (RNode _ _ _ (Just (x,y)) ) = (alphabet !! (x-1)) : show y
getMoveFromRNode (RNode _ _ _ Nothing      ) = ""


instance Game_tree RNode where
    --is_terminal :: RNode -> Bool
    is_terminal a = ([] == (children a))
    
    --node_value :: RNode -> Int
    node_value (RNode field p t _) = if p == t then nv else (-1 * nv)
        where nv = foldl (\acc (x,_) -> x+acc) 0 $ elems field
    
    --children :: RNode -> [RNode]
    children (RNode field pC pT lM) = makeChildrenFromMoves (getValidMoves field pT) field pC pT


createWeightedArray :: String -> (Array (Int, Int) String) -> (Array (Int, Int) (Int, String))
createWeightedArray pC field = listArray (bounds field) list
    where list = makeElemList pC (elems field) 


opponent :: String -> String
opponent player = if player == "W" then "B" else "W"


makeElemList :: String -> [String] -> [(Int, String)]
makeElemList _ [] = []
makeElemList pC ("*":xs) = (0,"*") : makeElemList pC xs
makeElemList pC (x:xs)   = if (pC == x) then (1,pC) : makeElemList pC xs else ((-1),opponent pC) : makeElemList pC xs


makeChildrenFromMoves :: [(Int,Int)] -> (Array (Int,Int) (Int,String)) -> String -> String -> [RNode]
makeChildrenFromMoves [] _ _ _ = []
makeChildrenFromMoves (x:xs) field pC pT = (RNode (makeNewField x pT pC field) pC (opponent pT) (Just x)) : makeChildrenFromMoves xs field pC pT


makeNewField :: (Int,Int) -> String -> String -> (Array (Int,Int) (Int,String)) -> (Array (Int,Int) (Int,String))
makeNewField i pT pC field = field // changes
    where
        changes = splitToChanges directions i $ map (lookInDirectionFrom field pT i) directions
        splitToChanges _ _ [] = []
        splitToChanges (y:ys)(r,c)(0:xs) = splitToChanges ys (r,c) xs
        splitToChanges ((dr,dc):ys)(r,c)(x:xs) = ((r+dr*x,c+dc*x), (head (makeElemList pC [pT]))) : splitToChanges ((dr,dc):ys)(r,c)((x-1):xs)


getValidMoves :: (Array (Int,Int) (Int,String)) -> String -> [(Int, Int)]
getValidMoves field pT =  throwOutZeros i $ map (lookInAllDirectionsFrom field pT) i
    where
        i = indices field
        throwOutZeros :: [(Int,Int)] -> [Int] -> [(Int,Int)]
        throwOutZeros []     _      = []
        throwOutZeros _      []     = []
        throwOutZeros (y:ys) (0:xs) = throwOutZeros ys xs
        throwOutZeros (y:ys) (x:xs) = y : throwOutZeros ys xs


lookInAllDirectionsFrom :: (Array (Int,Int) (Int,String)) -> String -> (Int,Int) -> Int
lookInAllDirectionsFrom field pT (r,c) = sum $ map (lookInDirectionFrom field pT (r,c)) directions


-- from any "*" in field goes in the direction defined by dirRow and dirCol as long as NOT pT (aka the other colour) is present. returns the number of coins flipped for that field in that direction.
-- is 0 if in the specified direction a) no stone of colour comes or b) a stone of same colour comes immediatly or c) a stone of same colour doesnt come after other coloured stones 
lookInDirectionFrom :: (Array (Int,Int) (Int,String)) -> String -> (Int,Int) -> (Int, Int) -> Int
lookInDirectionFrom field pT (r,c) (dirRow, dirCol) = if ((field ! (r,c)) == ( 0 ,"*")) then countIt (r+dirRow) (c+dirCol) 0 else 0
    where   checkNext nR nC = ((nR <= ubR) && (nC <= ubC) && (nR >=lbR) && (nC >= lbC))
            countIt nR' nC' acc = if (checkNext nR' nC')            then
                    if ((field ! (nR', nC')) == (0,"*"))            then 0      else
                    if ((field ! (nR', nC')) == (1,pT))             then acc    else
                    countIt (nR'+dirRow) (nC'+dirCol) (acc +1)                  else 0
                    
            ((lbR,lbC),(ubR, ubC)) = bounds field
