
module ArraySort where

import Data.Array (Array, bounds, listArray)
import Data.Array.ST (STArray, thaw, runSTArray, readArray, writeArray)
import Control.Monad.ST ( ST )
import Control.Monad.State (StateT, execStateT, get, put, lift, when)


swap :: STArray s Int a -> Int -> Int -> ST s ()
swap arr i j = do
    elem1 <- readArray arr i
    elem2 <- readArray arr j
    writeArray arr i elem2
    writeArray arr j elem1


partitionLoop :: (Ord a)
  => STArray s Int a -> a -> Int
  -> StateT Int (ST s) ()
partitionLoop arr pivotElement i = do
    pivotIndex <- get
    thisElement <- lift $ readArray arr i
    when (thisElement <= pivotElement) $ do
        lift $ swap arr i pivotIndex
        put (pivotIndex + 1)


partition :: (Ord a)
 => STArray s Int a -> Int -> Int
 -> ST s Int
partition arr start end = do
    pivotElement <- readArray arr start
    let pivotIndex_0 = start + 1
    finalPivotIndex <- execStateT
        (mapM (partitionLoop arr pivotElement) [(start+1)..(end-1)])
        pivotIndex_0
    swap arr start (finalPivotIndex - 1)
    return $ finalPivotIndex - 1


quicksort :: (Ord a) => Array Int a -> Array Int a
quicksort inputArr = runSTArray $ do
    stArr <- thaw inputArr
    let (minIndex, maxIndex) = bounds inputArr
    quicksort' minIndex (maxIndex + 1) stArr
    return stArr


quicksort' :: (Ord a)
  => Int -> Int -> STArray s Int a
  -> ST s ()
quicksort' start end stArr =
    when (start + 1 < end) $ do
        pivotIndex <- partition stArr start end
        quicksort' start pivotIndex stArr
        quicksort' (pivotIndex + 1) end stArr


-- | tests for quicksort of Array Int
testQuicksort :: [Bool]
testQuicksort = zipWith (==) (map (func . fst) tests) (map snd tests)
    where
    func = quicksort
    tests = [
        (listArray (0,4) [3,2,1,0,5], listArray (0,4) [0,1,2,3,5]),
        (listArray (0,4) [3,2,1,1,2], listArray (0,4) [1,1,2,2,3]),
        (listArray (0,4) [1,2,3,4,5], listArray (0,4) [1,2,3,4,5]),
        (listArray (0,4) [5,4,3,2,1], listArray (0,4) [1,2,3,4,5]),
        (listArray (0,4) [0,0,0,0,0], listArray (0,4) [0,0,0,0,0])
        ]


main :: IO ()
main = do
    print testQuicksort