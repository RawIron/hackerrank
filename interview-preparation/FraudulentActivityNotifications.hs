module Main where

import Data.Functor ( (<&>) )
import Data.List as L ( foldl' )
import Data.Vector ( Vector, (!) )
import qualified Data.Vector as V
import Data.Vector.Algorithms.Intro as VA ( sort )


readWindowSize :: IO Int
readWindowSize = do
    getLine <&> read . (!!1) . words

readExpenses :: IO [Int]
readExpenses = do
    getLine <&> map read . words

readInput :: IO (Vector Int, Int)
readInput = do
    window <- readWindowSize
    expenses <- readExpenses
    return (V.fromList expenses, window)


-- | moving median window
--
data MedianWindow = MedianWindow {
    msSortedRange :: Vector Int,
    msSizeIsEven :: Bool,
    msLeftMax :: Int,
    msMidPoint :: Int,
    msRightMin :: Int
} | EmptyMedianWindow deriving (Eq)


-- | create moving median window from a vector
--
fromVector :: Vector Int -> MedianWindow
fromVector window =
    MedianWindow sortedWindow (even n) (sortedWindow ! 0) midPoint (sortedWindow ! (n-1))
    where
    sortedWindow = V.modify VA.sort window
    n = V.length sortedWindow
    midPoint = n `div` 2


-- | update moving median window
--
updateMedianWindow :: MedianWindow -> Int -> Int -> MedianWindow
updateMedianWindow state deleteValue addValue
    | deleteValue == addValue = state
    | otherwise = MedianWindow sortedRange sizeIsEven setLeftMax setMidpoint setRightMin
    where
    sortedRange = msSortedRange state
    sizeIsEven = msSizeIsEven state
    leftMax = msLeftMax state
    midPoint = msMidPoint state
    rightMin = msRightMin state

    midPointValue = sortedRange ! midPoint
    
    setMidpoint
        --  +  - |
        | addValue < midPointValue && deleteValue < midPointValue = midPoint
        --       |   -   +
        | addValue > midPointValue && deleteValue > midPointValue = midPoint
        --    +  |   -
        | addValue < midPointValue && midPointValue < deleteValue = midPoint-1
        --    +  -
        | addValue < midPointValue && midPointValue == deleteValue = midPoint-1
        --    -  |    +
        | deleteValue < midPointValue && midPointValue < addValue = midPoint+1
        --       -    +
        | deleteValue == midPointValue && midPointValue < addValue = midPoint+1
        --     - +
        | addValue == midPointValue && deleteValue < midPointValue = midPoint
        --       +   -
        | addValue == midPointValue && midPointValue < deleteValue = midPoint

    setLeftMax
        | addValue == midPointValue && deleteValue < midPointValue = addValue
        | deleteValue == midPointValue && addValue > midPointValue = deleteValue + 1
        | bothLeft && leftMax < maxValue = maxValue
        | leftMax < addValue && addValue < midPointValue = addValue
        | leftMax <= deleteValue && deleteValue < midPointValue = deleteValue + 1
        | otherwise = leftMax
        where
        maxValue = max addValue (deleteValue + 1)
        bothLeft = addValue < midPointValue && deleteValue < midPointValue
    
    setRightMin
        | addValue == midPointValue && deleteValue > midPointValue = addValue
        | deleteValue == midPointValue && addValue < midPointValue = deleteValue - 1
        | bothRight && minValue < rightMin = minValue
        | midPointValue < addValue && addValue < rightMin = addValue
        | midPointValue < deleteValue && deleteValue <= rightMin = deleteValue - 1
        | otherwise = rightMin
        where
        minValue = min addValue (deleteValue - 1)
        bothRight = midPointValue < addValue && midPointValue < deleteValue


testUpdateMedianWindow :: [Bool]
testUpdateMedianWindow = zipWith (==)  have expected
    where
    expected = map snd tests
    have = map (func . fst) tests
    func = uncurry (updateMedianWindow $ MedianWindow (V.fromList [1,2,3,4,5,6,7]) False 1 3 7)
    tests = [
        --                                [1,3,3,4,5,6,7]
        --                                       |
        ((2, 3), MedianWindow (V.fromList [1,2,3,4,5,6,7]) False 3 3 7),

        --                                [1,2,3,4,6,7,9]
        --                                       |
        ((5, 9), MedianWindow (V.fromList [1,2,3,4,5,6,7]) False 1 3 4),

        --                              [1,2,3,3,4,5,6]
        --                                     | 
        ((7, 3), MedianWindow (V.fromList [1,2,3,4,5,6,7]) False 3 2 6),

        --                                  [1,3,4,5,6,6,7]
        --                                         |
        ((2, 6), MedianWindow (V.fromList [1,2,3,4,5,6,7]) False 3 4 6),

        --                                  [1,2,3,5,6,7,8]
        --                                         |
        ((4, 8), MedianWindow (V.fromList [1,2,3,4,5,6,7]) False 5 4 7),

        --                              [1,1,2,3,5,6,7]
        --                                     |
        ((4, 1), MedianWindow (V.fromList [1,2,3,4,5,6,7]) False 1 2 3),

        --                                [1,3,4,4,5,6,7]
        --                                       |
        ((2, 4), MedianWindow (V.fromList [1,2,3,4,5,6,7]) False 4 3 7),

        --                                [1,2,3,4,4,5,7]
        --                                       |
        ((6, 4), MedianWindow (V.fromList [1,2,3,4,5,6,7]) False 1 3 4)
        ]


-- | init moving median
--
movingMedian :: MedianWindow -> Vector Int -> (MedianWindow, Int)
movingMedian EmptyMedianWindow window = reset
    where
    reset = (resetState, medianTimes2 (msSortedRange resetState))
    resetState = fromVector window


-- | update moving median
--
movingMedian state window
    | leaveSortedRange = reset
    | exhaustSortedRange = reset
    | otherwise = (updatedState, setMedian)
    where
    reset = (resetState, medianTimes2 (msSortedRange resetState))
    resetState = fromVector $ V.slice 1 (V.length window - 1) window

    updatedState = updateMedianWindow state (window ! 0) insertValue
    insertValue = window ! (V.length window - 1)

    sortedRange = msSortedRange updatedState
    sizeIsEven = msSizeIsEven updatedState
    leftMax = msLeftMax updatedState
    midPoint = msMidPoint updatedState
    rightMin = msRightMin updatedState

    n = V.length sortedRange

    leaveSortedRange
        | sizeIsEven = midPoint < 1 || midPoint >= n
        | otherwise  = midPoint < 0 || midPoint >= n

    midPointValue = sortedRange ! midPoint
    exhaustSortedRange
        | sizeIsEven = midPointValue <= leftMax || rightMin < midPointValue
        | otherwise  = midPointValue < leftMax  || rightMin < midPointValue

    midPointPreValue = max leftMax (sortedRange ! (midPoint-1))
    setMedian
        | sizeIsEven = midPointValue + midPointPreValue
        | otherwise  = midPointValue * 2


testMovingMedian :: [Bool]
testMovingMedian = zipWith (==)  have expected
    where
    expected = map snd tests
    have = map (snd . func . fst) tests
    func = uncurry movingMedian
    tests = [
        -- uneven : left
        ((MedianWindow (V.fromList [1,2,3,4,5,6,7]) False 1 3 7, V.fromList [7,1]), 6),
        -- even : left
        ((MedianWindow (V.fromList [1,2,3,4,5,6]) True 1 3 6, V.fromList [6,1]), 5),
        -- even : right
        ((MedianWindow (V.fromList [1,2,3,4,5,6]) True 1 3 6, V.fromList [1,6]), 9),
        -- even : miss
        ((MedianWindow (V.fromList [1,2,3,4,5,6]) True 1 1 6, V.fromList [6,4,5,6,7,8,1]), 11),
        -- even : miss
        ((MedianWindow (V.fromList [1,2,3,4,5,6]) True 2 2 6, V.fromList [6,4,5,6,7,8,1]), 11)
        ]

testFoldMovingMedian :: [Bool]
testFoldMovingMedian = map run tests
    where
    run (initState, windows, expected) = (== expected) . snd . foldl' go initState $ windows
    go state = movingMedian (fst state)
    tests = [
        -- right, right
        --                                |
        --                                |   |,9,10]
        ((MedianWindow (V.fromList [1,2,3,4,5,6,7]) False 1 3 7, 8),
         [V.fromList [2,9], V.fromList [3,10]], 12),

        -- left, left
        --                                |
        --                        [1,2|   |
        ((MedianWindow (V.fromList [1,2,3,4,5,6,7]) False 1 3 7, 8),
         [V.fromList [7,1], V.fromList [6,2]], 4),

        -- leave right, right
        --                                      |
        --                                      |,9,10]
        ((MedianWindow (V.fromList [1,2,3,4,5,6,7]) False 1 6 7, 14),
         [V.fromList [1,4,5,6,7,8,9,9], V.fromList [4,10]], 16),

        -- leave left, left
        --                          |
        --                      [0,1|
        ((MedianWindow (V.fromList [1,2,3,4,5,6,7]) False 1 0 7, 2),
         [V.fromList [9,4,5,6,7,8,9,0], V.fromList [10,1]], 10),

        -- right, right, exhaust
        --                                |  
        --                                |   |,6,9]
        --                                |     |,7]
        ((MedianWindow (V.fromList [1,2,3,4,5,6,7]) False 1 3 7, 8),
         [V.fromList [2,6], V.fromList [3,9], V.fromList [1,4,5,6,7,8,9,7]], 14),

        -- left, left, exhaust
        --                                |
        --                        [1,2|   |
        --                       [1,|     |
        ((MedianWindow (V.fromList [1,2,3,4,5,6,7]) False 1 3 7, 8),
         [V.fromList [6,2], V.fromList [7,1], V.fromList [5,4,5,6,7,8,9,1]], 12),
        
        -- right, leave right, right,
        --
        -- (V.fromList [1,2,3,4,5,6,7,8,9], 4)
        ((MedianWindow (V.fromList [1,2,3,4]) True 1 2 4, 5),
         [V.fromList [1,5], V.fromList [2,3,4,5,6], V.fromList [3,7]], 11)
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


testMedianTimes2 :: [Bool]
testMedianTimes2 =
    zipWith (==) (map (func . fst) tests) (map snd tests)
    where
    func = medianTimes2
    tests = [
            (V.slice 1 4 $ V.fromList [2,3,4,2,3,6,8,4,5], 3+3),
            (V.fromList [40,50,30,10,20], 2*30)
        ]


-- | special case of windoSize is 1
-- pairwise comparison
countFor1 :: Vector Int -> Int
countFor1 expenses = fst . V.foldl' fraud initial $ V.tail expenses
    where
    initial = (0, expenses ! 0)
    fraud (counter, previous) expense
        | expense >= 2 * previous = (counter+1, expense)
        | otherwise = (counter, expense)


-- | every window is sorted
countNaive :: Vector Int -> Int -> Int
countNaive expenses windowSize =
    L.foldl' fraud 0 [0 .. (n - 1 - windowSize)]
    where
    n = V.length expenses
    fraud :: Int -> Int -> Int
    fraud counter i
        | expense >= threshold = counter+1
        | otherwise = counter
        where
        window = V.modify VA.sort $ V.slice i windowSize expenses
        threshold = medianTimes2 window
        expense = expenses ! (i+windowSize)


-- | reduce number of sorts
countMovingMedian :: Vector Int -> Int -> Int
countMovingMedian expenses windowSize =
    snd $ L.foldl' fraud (EmptyMedianWindow, 0) [0 .. (n - 1 - windowSize)]
    where
    n = V.length expenses
    fraud :: (MedianWindow, Int) -> Int -> (MedianWindow, Int)
    fraud (state, counter) i
        | expense >= threshold = (nextState, counter+1)
        | otherwise = (nextState, counter)
        where
        window
            | i > 0  = V.slice (i-1) (windowSize+1) expenses
            | i == 0 = V.slice 0 windowSize expenses
        (nextState, threshold) = movingMedian state window
        expense = expenses ! (i+windowSize)


-- | the median of previous expenses is used
-- to decide whether an expense is fraudulent
countNotifications :: Vector Int -> Int -> Int
countNotifications expenses windowSize
    | windowSize == V.length expenses = 0
    | windowSize == 1 = countFor1 expenses
    -- | windowSize < 10 = countNaive expenses windowSize
    | otherwise = countMovingMedian expenses windowSize


testCountNotifications :: [Bool]
testCountNotifications =
    zipWith (==) (map (func . fst) tests) (map snd tests)
    where
    func = uncurry countNotifications
    tests = [
            ((V.fromList [2,3,4,2,3,6,8,4,8], 1), 2),
            ((V.fromList [1,2,3,4,4], 5), 0),
            ((V.fromList [1,2,3,4,4], 4), 0),
            ((V.fromList [1,2,3,4,4], 3), 1),
            ((V.fromList [1,2,3,4,4], 2), 1),
            ((V.fromList [10,20,30,40,50], 3), 1),
            ((V.fromList [2,2,2,2,2,2,2,2,2], 4), 0),
            ((V.fromList [2,2,2,2,2,2,2,2,2], 3), 0),
            ((V.fromList [1,2,3,4,5,6,7,8,9], 5), 1),
            ((V.fromList [1,2,3,4,5,6,7,8,9], 4), 1),
            ((V.fromList [9,8,7,6,5,4,3,2,1], 5), 0),
            ((V.fromList [9,8,7,6,5,4,3,2,1], 4), 0),
            ((V.fromList [2,3,4,2,3,6,8,4,5], 5), 2),
            ((V.fromList [2,3,4,2,3,6,8,4,5], 4), 2)
        ]

runTests :: IO ()
runTests = do
    print testUpdateMedianWindow
    print testMovingMedian
    print testFoldMovingMedian
    print testMedianTimes2
    print testCountNotifications


solve :: IO ()
solve = do
    readInput >>=
        print . uncurry countNotifications


main :: IO ()
main = do
    -- runTests
    solve
