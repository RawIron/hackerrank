module Main where

import Control.Applicative
import Control.Monad
import System.IO


main :: IO ()
main = do
    numbers <- getMany :: IO [Int]
    let (counter,sorted) = bubbleSort numbers
    putStrLn $ "Array is sorted in " ++ show counter ++ " swaps."
    putStrLn $ "First Element: " ++ show (head sorted)
    putStrLn $ "Last Element: " ++ show (last sorted)


bubbleSort :: (Ord a, Integral b) => [a] -> (b,[a])
bubbleSort items = foldl (\(counter,acc) i -> bubbleOnce counter acc) (0,items) [1..length items]

bubbleOnce :: (Ord a, Integral b) => b -> [a] -> (b,[a])
bubbleOnce counter items = bubbleUp counter [] (head items) (tail items)

bubbleUp :: (Ord a, Integral b) => b -> [a] -> a -> [a]-> (b,[a])
bubbleUp counter swapped pivot [] = (counter, swapped ++ [pivot])
bubbleUp counter swapped pivot todo
    | pivot > head todo =
        bubbleUp (counter+1) (swapped ++ [head todo]) pivot (tail todo)
    | otherwise =
        bubbleUp counter (swapped ++ [pivot]) (head todo) (tail todo)


getMany :: (Read a) => IO [a]
getMany = do
    _ <- fmap read getLine :: IO Int
    fmap (map read . words) getLine
