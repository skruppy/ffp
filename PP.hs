-- "THE BEER-WARE LICENSE" (Revision 42):
-- Albatrouss and <skruppy@onmars.eu> wrote this software. As long as you retain
-- this notice you can do whatever you want with this stuff. If we meet some
-- day, and you think this stuff is worth it, you can buy me a beer in return.
--                                                     -- Albatrouss and Skruppy

module PP (prettyPrint) where

import Data.Array


prettyPrint :: (Array (Int, Int) String) -> IO ()
prettyPrint field = do
                        let (_,(_,size)) = bounds field
                        putStrLn $ firstLine size
                        printLines size size field
  
  
printLines :: Int -> Int ->(Array (Int, Int) String) -> IO()
printLines 1    len field = putStrLn (itemLine 1 len field)     >> putStrLn (finalLine len)     >> putStrLn (indexLine len)
printLines todo len field = putStrLn (itemLine todo len field)  >> putStrLn (fillerLine len)    >> printLines (todo -1) len field
    

itemLine :: Int -> Int ->(Array (Int, Int) String) -> String
itemLine line len field = number ++ (makeLinePretty $ getLineFromArray line len field)
    where number = if line < 10 then " " ++ (show line) else (show line)
        
        
makeLinePretty :: [String] -> String
makeLinePretty [] = "│"
makeLinePretty (x:xs) = "│" ++ newx ++ makeLinePretty xs
    where newx = if (x == "W")then " ⛂ " else if x == "B" then " ⛀ " else " * "

    
getLineFromArray :: Int -> Int -> (Array (Int, Int) String) -> [String]
getLineFromArray line size field = getElementsFromTo (line) size (elems field)


getElementsFromTo ::Int -> Int -> [a] -> [a]
getElementsFromTo line 0 _ = []
getElementsFromTo line size elems = (elems !! (line -1 )) : (getNextElem line size (size-1) elems)
    where   getNextElem _ _ 0 _ = []
            getNextElem line size todo elems = (elems !! ((line -1) + size * (size-todo))): (getNextElem line size (todo-1) elems)


finalLine :: Int -> String
finalLine len = ' ':' ':'└':'─':'─':'─': nextChars (len-1)
    where   nextChars 0 = '┘':[]
            nextChars 1 = '┴':'─':'─':'─':'┘':[]
            nextChars len' = '┴':'─':'─':'─':nextChars (len'-1)

indexLine :: Int -> String
indexLine len = ' ':' ':' ':' ':'A':' ': nextChars (len-1)
    where   nextChars 0 = []
            nextChars 1 = ' ':' ':(['A'..'Z'] !! (len - 1)):' ':' ':[]
            nextChars len' = ' ':' ':(['A'..'Z'] !! (len - len')):' ':nextChars (len'-1)

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