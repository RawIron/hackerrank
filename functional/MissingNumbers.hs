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

input :: IO [[Int]]
input = do
    getContents >>=
        return . map strToInts . keepOnlyList . lines
    where
        keepOnlyList = map snd . filter fst . (zip [False, True, False, True])
        strToInts = map (read :: String -> Int) . words

main :: IO ()
main = do
    (listA : listB : _) <- input
    putStrLn . unwords . map show $ solve listA listB
