module Main where

import Data.Map as Map
import Data.List as List

-- | decrement the count of a key in a frequency table
-- > freqDecr 7 Map.fromList [(7,2),(2,1)] == Map.fromList [(7,1),(2,1)]
freqDecr :: Int -> Map Int Int -> Map Int Int
freqDecr k freqTable = Map.update f k freqTable
  where
  f x = if x > 1 then Just (x-1) else Nothing

-- | build frequency table of numbers in a list
-- > frequencies [3,4,2,3] == Map.fromList [(2,1),(3,2),(4,1)]
frequencies :: [Int] -> Map Int Int
frequencies numbers = Map.fromListWith (+) $ zip numbers (repeat 1)

-- | elements in b which are not in a
-- 
-- count elements in b
-- read element from a and decrement the count in b
-- elements in b with a count > 0 are not in a
--
-- > notIn [3,3] [3] = [3]
-- > notIn [2,3] [3] = [2]
-- > notIn [2,3] [3,3] = [2]
notIn :: [Int] -> [Int] -> [Int]
notIn b a = Map.keys . Map.filter (> 0) $ List.foldl (flip freqDecr) (frequencies b) a

-- | parse input string from stdin
input :: IO ([Int], [Int])
input = do
  _ <- readLn :: IO Int
  a <- (getLine >>= return . List.map read . words)
  _ <- readLn :: IO Int
  b <- (getLine >>= return . List.map read . words)
  return (b, a)

main :: IO ()
main = do
  input >>= mapM_ (putStr . (++ " ") . show) . uncurry notIn
