import Data.Tree.Game_tree.Negascout as NS
import Data.Tree.Game_tree.Game_tree 
import Data.Array

alphabet = ['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z'];            
            
--              Up   UpRight Right DownRight Down DownLeft Left   UpLeft
directions = [(-1,0),(-1,1), (0,1),(1,1),    (1,0),(1,-1), (0,-1),(-1,-1)]

searchdepth = 5  

data RNode = 
    RNode  {gamefield       :: (Array (Int, Int) (Int, String)),
            playerColour    :: String, -- playerNumber 0 => B; 1 => W
            playerTurn      :: String, --whose turn it is --> B/W
            lastMove        :: Maybe (Int,Int)
            } deriving (Eq, Show)

prettyPrint :: (Array (Int, Int) String) -> IO ()
prettyPrint field = do
                        let (_,(_,size)) = bounds field
                        putStrLn $ firstLine size
                        printLines size size field
  
  
printLines :: Int -> Int ->(Array (Int, Int) String) -> IO()
printLines 1 len field = putStrLn (itemLine 1 len field) >> putStrLn (finalLine len) >> putStrLn (indexLine len)
printLines todo len field = (putStrLn (itemLine todo len field)) >> (putStrLn (fillerLine len)) >> (printLines (todo -1) len field)


itemLine :: Int -> Int ->(Array (Int, Int) String) -> String
itemLine line len field = number ++ (makeLinePretty $ getLineFromArray line len field)
    where number = if line < 10 then " " ++ (show line) else (show line)
        
        
makeLinePretty :: [String] -> String
makeLinePretty [] = "│"
makeLinePretty (x:xs) = "│" ++ newx ++ makeLinePretty xs
    where newx = if (x == "W")then " ⛀ " else if x == "B" then " ⛂ " else " * "

    
getLineFromArray :: Int -> Int -> (Array (Int, Int) String) -> [String]
getLineFromArray line size field = getElementsFromTo (line*size) ((line+1)*size) (elems field)


getElementsFromTo ::Int -> Int -> [a] -> [a]
getElementsFromTo 0 0 _ = []
getElementsFromTo 0 totake (x:xs) = x: getElementsFromTo 0 (totake - 1) xs
getElementsFromTo x totake list = getElementsFromTo (x-1) totake list


finalLine :: Int -> String
finalLine len = ' ':' ':'└':'─':'─':'─': nextChars (len-1)
    where   nextChars 0 = '┘':[]
            nextChars 1 = '┴':'─':'─':'─':'┘':[]
            nextChars len' = '┴':'─':'─':'─':nextChars (len'-1)

indexLine :: Int -> String
indexLine len = ' ':' ':' ':' ':'A':' ': nextChars (len-1)
    where   nextChars 0 = []
            nextChars 1 = ' ':' ':(alphabet !! (len - 1)):' ':' ':[]
            nextChars len' = ' ':' ':(alphabet !! (len - len')):' ':nextChars (len'-1)

firstLine:: Int -> String
firstLine len = ' ':' ':'┌':'─':'─':'─': nextChars (len-1)
    where   nextChars 0 = '┐':[]
            nextChars 1 = '┬':'─':'─':'─':'┐':[]
            nextChars len' = '┬':'─':'─':'─':nextChars (len'-1)

            
fillerLine:: Int -> String
fillerLine len = ' ':' ':'├':'─':'─':'─': nextChars (len-1)
    where   nextChars 0 = '┤':[]
            nextChars 1 = '┼':'─':'─':'─':'┤':[]
            nextChars len' = '┼':'─':'─':'─':nextChars (len'-1)
          
            
getNextMove :: (Array (Int, Int) String) -> String -> String
getNextMove field pC = getMoveFromRNode $ getNextRNode a searchdepth
    where a = (RNode (createWeightedArray pC field) pC pC Nothing)
            
getNextRNode :: RNode -> Int -> RNode
getNextRNode a i = res !! 1
    where (res, _) = alpha_beta_search a i

            
getMoveFromRNode:: RNode -> String
getMoveFromRNode (RNode _ _ _ (Just (x,y))) = (show(alphabet !! (x-1))) ++ show y
getMoveFromRNode (RNode _ _ _ Nothing) = ""

            
instance Game_tree RNode where
                   
    --is_terminal :: RNode -> Bool
    is_terminal a = ([] == (children a))

    --node_value :: RNode -> Int
    node_value (RNode field p t _) = if p == t then nv else (-1 * nv)
        where   nv = foldl (\acc (x,_) -> x+acc) 0 $ elems field
    
    --children :: RNode -> [RNode]
    children (RNode field pC pT lM) = makeChildrenFromMoves (getValidMoves field pT) field pC pT


createWeightedArray :: String -> (Array (Int, Int) String) -> (Array (Int, Int) (Int, String))
createWeightedArray pC field = listArray (bounds field) list
    where   list = makeElemList pC (elems field) 


makeElemList :: String -> [String] -> [(Int, String)]
makeElemList _ [] = []
makeElemList pC ("*":xs) = (0,"*") : makeElemList pC xs
makeElemList pC (x:xs) = if (pC == x) then (1,pC) : makeElemList pC xs else ((-1),notPC) : makeElemList pC xs
    where   notPC = if pC == "W" then "B" else "W"


makeChildrenFromMoves :: [(Int,Int)] -> (Array (Int,Int) (Int,String)) -> String -> String -> [RNode]
makeChildrenFromMoves [] _ _ _ = []
makeChildrenFromMoves (x:xs) field pC pT = (RNode (makeNewField x pT pC field) pC notPT (Just x)) : makeChildrenFromMoves xs field pC pT
    where   notPT = if pT == "W" then "B" else "W"

    
makeNewField :: (Int,Int) -> String -> String -> (Array (Int,Int) (Int,String)) -> (Array (Int,Int) (Int,String))
makeNewField i pT pC field = field // changes
    where   changes = splitToChanges directions i $ map (lookInDirectionFrom field pT i) directions
            splitToChanges _ _ [] = []
            splitToChanges (y:ys)(r,c)(0:xs) = splitToChanges ys (r,c) xs
            splitToChanges ((dr,dc):ys)(r,c)(x:xs) = ((r+dr*x,c+dc*x), (head (makeElemList pC [pT]))) : splitToChanges ((dr,dc):ys)(r,c)((x-1):xs)
    

getValidMoves :: (Array (Int,Int) (Int,String)) -> String -> [(Int, Int)]
getValidMoves field pT =  throwOutZeros i $ map (lookInAllDirectionsFrom field pT) i
    where   i = indices field
            throwOutZeros :: [(Int,Int)] -> [Int] -> [(Int,Int)]
            throwOutZeros [] _ = []
            throwOutZeros _ [] = []
            throwOutZeros (y:ys) (0:xs) = throwOutZeros ys xs
            throwOutZeros (y:ys) (x:xs) = y : throwOutZeros ys xs

    
lookInAllDirectionsFrom :: (Array (Int,Int) (Int,String)) -> String -> (Int,Int) -> Int
lookInAllDirectionsFrom field pT (r,c) = sum $ map (lookInDirectionFrom field pT (r,c)) directions
    

-- from any "*" in field goes in the direction defined by dirRow and dirCol as long as NOT pT (aka the other colour) is present. returns the number of coins flipped for that field in that direction.
-- is 0 if in the specified direction a) no stone of colour comes or b) a stone of same colour comes immediatly or c) a stone of same colour doesnt come after other coloured stones 
lookInDirectionFrom :: (Array (Int,Int) (Int,String)) -> String -> (Int,Int) -> (Int, Int) -> Int
lookInDirectionFrom field pT (r,c) (dirRow, dirCol) = if ((field ! (r,c)) == ( 0 ,"*")) then countIt (r+dirRow) (c+dirCol) 0 else 0
    where   checkNext nR nC = if ((nR <= ubR) && (nC <= ubC) && (nR >=lbR) && (nC >= lbC)) then True else False 
            countIt nR' nC' acc = if (checkNext nR' nC')            then 
                    if ((field ! (nR', nC')) == (0,"*"))            then 0      else
                    if ((field ! (nR', nC')) == (1,pT)&& acc >0)    then acc    else
                    countIt (nR'+dirRow) (nC'+dirCol) (acc +1)                  else 0 
                    
            ((lbR,lbC),(ubR, ubC)) = bounds field
                  
