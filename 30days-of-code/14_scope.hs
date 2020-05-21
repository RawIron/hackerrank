module Main where

import Control.Applicative
import Control.Monad
import System.IO


main :: IO ()
main = do
    numbers <- getMany :: IO [Int]
    let distance = maxDistance numbers
    putStrLn $ show distance


maxDistance :: (Ord a, Integral a) => [a] -> a
maxDistance items = high - low
  where (low,high) = minmax items

minmax :: (Ord a, Integral a) => [a] -> (a,a)
minmax (x:xs) = foldl (\(low,high) item -> (min low item, max high item)) (x, x) xs


getMany :: (Read a) => IO [a]
getMany = do
    _ <- fmap read getLine :: IO Int
    fmap (map read . words) getLine
