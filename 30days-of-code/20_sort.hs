module Main where

import Control.Applicative
import Control.Monad
import System.IO


main :: IO ()
main = do
    numbers <- getMany
    let (counter:sorted) = bubbleSort numbers
    putStrLn $ "Array is sorted in " ++ show counter ++ " swaps."
    putStrLn $ "First Element: " ++ show (head sorted)
    putStrLn $ "Last Element: " ++ show (last sorted)


bubbleSort :: [Int] -> [Int]
bubbleSort numbers = foldl (\(counter:acc) i -> bubbleOnce counter acc) (0:numbers) [1 .. length numbers]

bubbleOnce :: Int -> [Int] -> [Int]
bubbleOnce counter numbers = bubbleUp counter [] (head numbers) (tail numbers)

bubbleUp :: Int -> [Int] -> Int -> [Int] -> [Int]
bubbleUp counter swapped pivot [] = counter : swapped ++ [pivot]
bubbleUp counter swapped pivot todo
    | pivot > head todo =
        bubbleUp (counter+1) (swapped ++ [head todo]) pivot (tail todo)
    | otherwise =
        bubbleUp counter (swapped ++ [pivot]) (head todo) (tail todo)


getMany :: IO [Int]
getMany = do
    _ <- fmap read getLine :: IO Int
    fmap (map read . words) getLine :: IO [Int]

getMultipleLines :: Int -> IO [String]
getMultipleLines n
    | n <= 0 = return []
    | otherwise = do          
        x <- getLine         
        xs <- getMultipleLines (n-1)    
        let ret = (x:xs)    
        return ret
