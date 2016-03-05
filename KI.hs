import Data.Tree.Game_tree.Negascout
import Data.Tree.Game_tree.Game_tree
import Data.Array


data RNode = 
    RNode { gamefield       :: (Array (Int, Int) (Int, String)),
            playerColour    :: String, -- playerNumber 0 => B; 1 => W
            playerTurn      :: String --whose turn it is --> B/W
            } deriving Show 
            
is_terminal :: RNode -> Bool
is_terminal a = (countUp a) && (countUpRight a) && (countRight a) && (countDownRight a) && (countDown a) && (countDownLeft a) && (countLeft a) && (countUpLeft a)


countUp         a = countB    (-1)    0       a
countUpRight    a = countB    (-1)    1       a
countRight      a = countB    0       1       a
countDownRight  a = countB    1       1       a
countDown       a = countB    1       0       a
countDownLeft   a = countB    1       (-1)    a
countLeft       a = countB    0       (-1)    a
countUpLeft     a = countB    (-1)    (-1)    a

--countB ::  Int -> Int -> RNode -> Bool
countB rC cC (RNode field p t) = False



node_value :: RNode -> Int
node_value (RNode field p t) = if p == t then nv else (-1 * nv)
    where nv = foldl (\acc (x,_) -> x+acc) 0 $ elems field



createWeightedArray :: (Array (Int, Int) String) -> (Array (Int, Int) (Int, String))
createWeightedArray field = listArray (bounds field) list
    where list = makeElemList $ elems field
          
    
makeElemList :: [String] -> [(Int, String)]
makeElemList [] = []
makeElemList ("*":xs) = (0,"*") : makeElemList xs
makeElemList ("W":xs) = (1,"W") : makeElemList xs
makeElemList ("B":xs) = ((-1),"B") : makeElemList xs
makeElemList (x:xs) = (minBound,x) : makeElemList xs


--TODO: children :: RNode -> [RNode]