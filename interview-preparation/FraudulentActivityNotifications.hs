module Main where

import Data.Functor ( (<&>) )
import Data.List as L ( foldl' )
import Data.Vector ( Vector, (!) )
import qualified Data.Vector as V
import Data.Vector.Algorithms.Intro as VA ( sort )
import Text.ParserCombinators.ReadPrec (reset)


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

-- | use values of a triple as function arguments
-- > uncurry3 (\x y z -> x + y + z) (1,2,3) == 6
uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f ~(a, b, c) = f a b c


data MedianStream = MedianStream {
    msSortedRange :: Vector Int,
    msSize :: Int,
    msLeftMax :: Int,
    msMidPoint :: Int,
    msRightMin :: Int
} | Empty

fromVector :: Vector Int -> MedianStream
fromVector window =
    MedianStream sortedWindow n (sortedWindow ! 0) midPoint (sortedWindow ! (n-1))
    where
    sortedWindow = V.modify VA.sort window
    n = length sortedWindow
    nHalf = n `div` 2
    midPoint
        | even n = nHalf-1
        | otherwise = nHalf


medianS :: MedianStream -> Vector Int -> (MedianStream, Int)
medianS Empty window = reset
    where
    reset = (resetState, medianTimes2 (msSortedRange resetState))
    resetState = fromVector window
  
medianS state window
    | leaveLeft = reset
    | blocksLeft = reset
    | leaveRight = reset
    | blocksRight = reset
    | isLeft = moveLeft
    | isRight = moveRight
    where
    reset = (resetState, medianTimes2 (msSortedRange resetState))
    resetState = fromVector window
    value = window ! (length window - 1)

    size = msSize state
    sortedRange = msSortedRange state
    leftMax = msLeftMax state
    rightMin = msRightMin state
    midPoint = msMidPoint state
    n = length sortedRange

    midPointValue = sortedRange ! midPoint
    midPointPreValue = max leftMax (sortedRange ! (midPoint-1))
    midPointSucValue = min rightMin (sortedRange ! (midPoint+1))

    leaveLeft = midPoint == 0 && value < midPointValue
    leaveRight =  midPoint == n-1 && value > midPointValue
    blocksLeft = midPointPreValue <= value && value <= midPointValue
    blocksRight = midPointValue <= value && value <= midPointSucValue

    isLeft = value < midPointPreValue
    isRight = value > midPointSucValue
    isSizeEven = even (size+1)

    moveLeft  = (MedianStream sortedRange (size+1) setLeftMax setLeftMoveMidpoint setRightMin, setLeftMoveMedian)
    setLeftMoveMedian
        | isSizeEven = midPointValue + midPointPreValue
        | otherwise = midPointValue * 2
    setLeftMoveMidpoint
        | isSizeEven = midPoint
        | otherwise = midPoint - 1

    moveRight = (MedianStream sortedRange (size+1) setLeftMax setRightMoveMidpoint setRightMin, setRightMoveMedian)
    setRightMoveMedian
        | isSizeEven = midPointValue + midPointSucValue
        | otherwise = midPointValue * 2 
    setRightMoveMidpoint
        | isSizeEven = midPoint + 1
        | otherwise = midPoint

    setLeftMax
        | isLeft && value > leftMax = value
        | otherwise = leftMax
    setRightMin
        | isRight && value < rightMin = value
        | otherwise = rightMin


testMedianS :: [Bool]
testMedianS = zipWith (==)  have expected
    where
    expected = map snd tests
    have = map (snd . func . fst) tests
    func = uncurry medianS
    tests = [
        -- leavesLeft
        ((MedianStream (V.fromList [2,3]) 3 2 0 4, V.fromList [2,3,4,1]), 5),
        -- leavesRight
        ((MedianStream (V.fromList [2,3]) 3 2 1 4, V.fromList [2,4,4,5]), 8),
        -- blocksLeft
        ((MedianStream (V.fromList [1,3,5]) 3 1 1 5, V.fromList [1,3,4,2]), 5),
        -- blocksRight
        ((MedianStream (V.fromList [1,3,5]) 3 1 1 5, V.fromList [1,2,3,5,4]), 6),
        -- moveRight, size+1 is even
        ((MedianStream (V.fromList [1,3,5]) 3 1 1 5, V.fromList [3,5,6]), 8),
        -- moveRight, size+1 is uneven
        ((MedianStream (V.fromList [1,2,3]) 4 1 1 5, V.fromList [3,5,4]), 4),
        -- moveLeft, size+1 is even
        ((MedianStream (V.fromList [1,3,5]) 3 1 1 5, V.fromList [3,5,0]), 4),
        -- moveLeft, size+1 is uneven
        ((MedianStream (V.fromList [1,3,3]) 4 1 1 5, V.fromList [3,5,0]), 6)
        ]

testFoldMedianS :: [Bool]
testFoldMedianS = map run tests
    where
    run (initState, windows, expected) = (== expected) . snd . foldl' go initState $ windows
    go state = medianS (fst state)
    tests = [
        -- moveRight moveRight
        --                                |
        --                                |   |,9,10]
        ((MedianStream (V.fromList [3,4,5,6,7,8]) 6 3 3 8, 0),
         [V.fromList [2,3,9], V.fromList [4,10]], 13),
        -- moveLeft moveLeft
        --                                |
        --                     [1,2,|     |
        ((MedianStream (V.fromList [3,4,5,6,7,8]) 6 3 3 8, 0),
         [V.fromList [2,3,2], V.fromList [4,1]], 9),
        -- leavesRight moveRight
        --                                    |
        --                                    |,9,10]
        ((MedianStream (V.fromList [3,4,5,6,7,8]) 9 3 5 8, 0),
         [V.fromList [2,4,1,3,9], V.fromList [1,2,3,4,9,10]], 7),
        -- leavesLeft moveLeft
        --                          |
        --                      [1,2|
        ((MedianStream (V.fromList [3,4,5,6,7,8]) 9 3 0 8, 0),
         [V.fromList [2,4,1,3,2], V.fromList [1,2,2,3,4,0]], 4),
        -- moveRight moveRight blocksRight
        --                              |  
        --                              |   |,6,8]
        --                              |  |,4]
        ((MedianStream (V.fromList [2,2,3,3,4]) 5 2 2 4, 0),
         [V.fromList [3,4,2,3,6], V.fromList [4,2,3,6,8], V.fromList [2,3,6,8,4]], 8)
        ]


-- | median of vector multiplied by 2
--
-- assert vector is sorted
medianTimes2 :: Vector Int -> Int
medianTimes2 window
        | even n = window ! nHalf + window ! (nHalf-1)
        | otherwise = (window ! nHalf) * 2
    where
    n = V.length window
    nHalf = n `div` 2


testMedian :: [Bool]
testMedian =
    zipWith (==) (map (func . fst) tests) (map snd tests)
    where
    func = medianTimes2
    tests = [
            (V.slice 1 4 $ V.fromList [2,3,4,2,3,6,8,4,5], 3+3),
            (V.fromList [40,50,30,10,20], 2*30)
        ]


countFor1 :: Vector Int -> Int
countFor1 expenses = fst . V.foldl' fraud initial $ V.tail expenses
    where
    initial = (0, expenses ! 0)
    fraud (counter, previous) expense
        | expense >= 2 * previous = (counter+1, expense)
        | otherwise = (counter, expense)


countNaive :: Vector Int -> Int -> Int
countNaive expenses windowSize =
    L.foldl' fraud i0 [i0 .. (n - 1 - windowSize)]
    where
    i0 = 0 :: Int
    n = V.length expenses
    fraud :: Int -> Int -> Int
    fraud counter i
        | expense >= threshold = counter+1
        | otherwise = counter
        where
        window = V.modify VA.sort $ V.slice i windowSize expenses
        threshold = medianTimes2 window
        expense = expenses ! (i+windowSize)


countStream :: Vector Int -> Int -> Int
countStream expenses windowSize =
    snd $ L.foldl' fraud (Empty, 0) [0 .. (n - 1 - windowSize)]
    where
    n = V.length expenses
    fraud :: (MedianStream, Int) -> Int -> (MedianStream, Int)
    fraud (state, counter) i
        | expense >= threshold = (nextState, counter+1)
        | otherwise = (nextState, counter)
        where
        window = V.slice i windowSize expenses
        (nextState, threshold) = medianS state window
        expense = expenses ! (i+windowSize)


countNotifications :: Vector Int -> Int -> Int
countNotifications expenses windowSize
    | windowSize == V.length expenses = 0
    | windowSize == 1 = countFor1 expenses
    -- | windowSize < 25 = countNaive expenses windowSize
    | otherwise = countStream expenses windowSize


testCountNotifications :: [Bool]
testCountNotifications =
    zipWith (==) (map (func . fst) tests) (map snd tests)
    where
    func = uncurry countNotifications
    tests = [
            -- 
            ((V.fromList [2,3,4,2,3,6,8,4,5], 5), 2),
            ((V.fromList [1,2,3,4,4], 4), 0),
            ((V.fromList [10,20,30,40,50], 3), 1),
            ((V.fromList [2,3,4,2,3,6,8,4,8], 1), 2)
        ]

runTests :: IO ()
runTests = do
    print testMedianS
    print testFoldMedianS
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
