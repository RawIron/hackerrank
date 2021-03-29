module Main where

import Control.Monad
import qualified Data.List as List
import qualified Data.Map as Map

-- | replace each element with its rank
-- > replaceWithRank [4,1,7] == [2,1,3]
replaceWithRank :: [Int] -> [Int]
replaceWithRank numbers = foldr go [] numbers
    where
    ranks = Map.fromList $ zip (List.sort numbers) [1..]
    go x acc = case (Map.lookup x ranks) of
        Just r -> r : acc
        Nothing -> acc

-- | increment the first element of list by k
-- > incr 2 [3,4,1] == [5,4,1]
incr :: Int -> [Int] -> [Int]
incr _ [] = []
incr k (x:xs) = (x+k:xs)

-- | 
-- > countMinBribes [3,1,2] == 2
-- > countMinBribes [3,2,1] == 3
countMinBribes :: [Int] -> Maybe Int
countMinBribes queue = countBribes (Just [], queue)
    where
    countBribes :: (Maybe [Int], [Int]) -> Maybe Int
    countBribes (bribes, queue)
        | bribes == Just [] = countBribes $ countBribes' bribes queue
        | bribes == Nothing = Nothing
        | (head <$> bribes) == Just 0 = sum <$> bribes
        | otherwise = countBribes $ countBribes' bribes queue

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

-- | 
showBribes :: Maybe Int -> String
showBribes (Just b) = show b
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
