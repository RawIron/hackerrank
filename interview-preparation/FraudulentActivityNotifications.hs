module Main where

import Data.Functor ( (<&>) )
import Data.List as L ( foldl' )
import Data.Vector ( Vector, (!) )
import qualified Data.Vector as V
import Data.Vector.Algorithms.Intro as VA ( sort )


readWindow :: IO Int
readWindow = do
    getLine <&> read . (!!1) . words
    
readExpenses :: IO [Int]
readExpenses = do
    getLine <&> map read . words
    
readInput :: IO (Vector Int, Int)
readInput = do
    window <- readWindow
    expenses <- readExpenses
    return (V.fromList expenses, window)


medianTimes2 :: Vector Int -> Int
medianTimes2 window = medianTimes2' $ V.modify VA.sort window
    where
    n = V.length window
    nHalf = n `div` 2
    medianTimes2' ws
        | n `mod` 2 == 1 = (ws ! nHalf) * 2
        | otherwise = ws ! nHalf + ws ! (nHalf-1)


countNotifications :: Vector Int -> Int -> Int
countNotifications expenses windowSize =
    L.foldl' fraud i0 [i0 .. (n - 1 - windowSize)]
    where
    i0 = 0 :: Int
    n = V.length expenses
    fraud :: Int -> Int -> Int
    fraud counter i
        | expense >= threshold = counter+1
        | otherwise = counter
        where
        window = V.slice i windowSize expenses
        threshold = medianTimes2 window
        expense = expenses ! (i+windowSize)


testMedian :: [Bool]
testMedian =
    zipWith (==) (map (func . fst) tests) (map snd tests)
    where
    func = medianTimes2
    tests = [
            (V.slice 1 4 $ V.fromList [2,3,4,2,3,6,8,4,5], 3+3),
            (V.fromList [40,50,30,10,20], 2*30)
        ]

testCountNotifications :: [Bool]
testCountNotifications =
    zipWith (==) (map (func . fst) tests) (map snd tests)
    where
    func = uncurry countNotifications
    tests = [
            ((V.fromList [2,3,4,2,3,6,8,4,5], 5), 2),
            ((V.fromList [1,2,3,4,4], 4), 0),
            ((V.fromList [10,20,30,40,50], 3), 1)
        ]

runTests :: IO ()
runTests = do
    print testMedian
    print testCountNotifications


solve :: IO ()
solve = do
    readInput >>=
        print . uncurry countNotifications

main :: IO ()
main = do
    runTests
    -- solve
