module Main where

import Data.Map (Map)
import qualified Data.Map as Map

-- superList \\ subList
-- [1..10] \\ [2,5,9] is implemented as delete 2 . delete 5 . delete 9 $ [1..10]
-- and that is too slow
import Data.List as L
useDiff :: [Int] -> [Int] -> [Int]
useDiff subList superList = sort . nub $ superList \\ subList

useMap :: [Int] -> [Int] -> [Int]
useMap subList superList =
    Map.keys . Map.filter (>0) $ foldl minus superCounter subList
    where
        superCounter = Map.fromListWith (+) $ zip superList (repeat 1)
        minus acc k = Map.updateWithKey (\k v -> Just (v-1)) k acc 

solve :: [Int] -> [Int] -> [Int]
solve = useMap

input :: IO ([Int], [Int])
input = do
    _ <- getLine
    lineA <- getLine
    let listA = map read . words $ lineA
    _ <- getLine
    lineB <- getLine
    let listB = map read . words $ lineB
    return (listA, listB)

main :: IO ()
main = do
    (listA, listB) <- input
    putStrLn . unwords . map show $ solve listA listB
