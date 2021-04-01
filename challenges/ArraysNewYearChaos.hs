module Main where

import Control.Monad
import qualified Data.List as List
import qualified Data.Map as Map

-- | replace each element with its rank
-- > replaceWithRank [4,1,7] == [2,1,3]
replaceWithRank :: (Ord a) => [a] -> [Int]
replaceWithRank xs = foldr go [] xs
    where
    ranks = Map.fromList $ zip (List.sort xs) [1..]
    go x acc = case (Map.lookup x ranks) of
        Just rank -> rank : acc
        Nothing -> acc

-- | increment the first element of list by k
-- > incr 2 [3,4,1] == [5,4,1]
incr :: (Num a) => a -> [a] -> [a]
incr _ [] = []
incr k (x:xs) = (x+k:xs)

-- | count how many times numbers were swapped with their left neighbour
--
-- split the list in two groups:
--  numbers which moved closer to the head
--  numbers which stayed at their position or moved towards the tail
--  for 3 2 4 1 the first group is 3 4 and the second is 2 1
-- count the swaps with their left neighbours for the first group
--  3 2 4 1
--  1 2 3 4
--  3 had (3-1) swaps and 4 had (4-3) swaps
-- replace the numbers in the second list with their ranks and start again
--  2 1 are already the ranks
-- done when the swap count in the first group is 0
--
-- > countMinBribes [3,1,2] == Just 2
-- > countMinBribes [3,2,1] == Just 3
-- > countMinBribes [1,2,5,3,7,8,6,4] == Just 7
countMinBribes :: [Int] -> Maybe Int
countMinBribes queue = countBribes (Just [], queue)
    where
    countBribes :: (Maybe [Int], [Int]) -> Maybe Int
    countBribes (bribes, queue)
        | bribes == Just [] = countBribes $ countBribes' bribes queue
        | bribes == Nothing = Nothing
        | (head <$> bribes) == Just 0 = sum <$> bribes
        | otherwise = countBribes $ countBribes' bribes queue

-- | count the swaps which moved a number towards the head
--   and create a new queue from items which stayed in position
--   or moved towards the tail
--
-- for [1,2,5,3,7,8,6,4] countBribes calls:
-- > countBribes' (Just []) [1,2,5,3,7,8,6,4] == (Just [6],[1,2,3,5,4])
-- > countBribes' (Just [6]) [1,2,3,5,4] == (Just [1,6],[1,2,3,4])
-- > countBribes' (Just [1,6]) [1,2,3,4] == (Just [0,1,6],[1,2,3,4])
countBribes' :: Maybe [Int] -> [Int] -> (Maybe [Int], [Int])
countBribes' bribes queue = (newBribes, newQueue)
    where
    (newBribes, stayedOrBack) = foldl go ((0:) <$> bribes, []) $ zip queue [1..]
    newQueue = (replaceWithRank . reverse) $ stayedOrBack
    go :: (Maybe [Int], [Int]) -> (Int, Int) -> (Maybe [Int], [Int])
    go (bribes, stayedOrBack) (after, before)
        | bribes == Nothing = (bribes, stayedOrBack)
        | (after - before) > 2 = (Nothing, stayedOrBack)
        | after > before = ((incr (after - before)) <$> bribes, stayedOrBack)
        | otherwise = (bribes, after : stayedOrBack)

-- | hackerrank output
showBribes :: Maybe Int -> String
showBribes (Just bribes) = show bribes
showBribes Nothing = "Too chaotic"

input :: IO [Int]
input = do
    n <- readLn :: IO Int
    getLine >>= return . map read . words

main :: IO()
main = do
    t <- readLn :: IO Int
    forM_ [1..t] $ \t_itr -> do
        input >>= putStrLn . showBribes . countMinBribes
