module Main where

import Control.Arrow ( (&&&) )
import Data.Functor ( (<&>) )
import qualified Data.List as L
import Data.Vector ( Vector )
import qualified Data.Vector as V
import Data.Map ( Map, (!) )
import qualified Data.Map as M


-- | use values of a triple as function arguments
-- > uncurry3 (\x y z -> x + y + z) (1,2,3) == 6
uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f ~(a, b, c) = f a b c


-- | the window, the median of this window and all data
--   needed to recalculate the median effeciently
--
-- midpoint
--  uneven size = the element with same number of elements
--                to left and right
--                      |
--                  1 2 3 4 5
--
--  even size = second of the pair with same number of
--              elements to left and right
--                       |
--                   1 2 3 4
--
data MovingMedian = MovingMedian {
        mwindow :: Map Int Int,
        mmSizeIsEven :: Bool,
        mmMidpoint :: Int,
        mmOffset :: Int
        } | EmptyMovingMedian

data Direction = LEFT | RIGHT | STAY deriving (Eq)


flatten :: MovingMedian -> (Map Int Int, Bool, Int, Int)
flatten state = (window, sizeIsEven, midpoint, offset)
    where
    window = mwindow state
    sizeIsEven = mmSizeIsEven state
    midpoint = mmMidpoint state
    offset = mmOffset state


-- | compress a vector into a map with frequency counts
--   store the midpoint of the vector as a (key, offset)
--   pair with the map
--
--    1 4 3 7 8 1 8 8 1 4       input vector
--
--              +-------------- Position of Midpoint
--              |
--    1 1 1 3 4 4 7 8 8 8
--              |
--            4,2               Key 4, Offset 2
--        3,1   | 7,1
--     1,3      |    8,3
--              |
--    1 2 3 4 5 6 7 8 9 10      index starting from 1
--
fromVector :: Vector Int -> MovingMedian
fromVector inVector = MovingMedian window sizeIsEven midpoint offset
    where
    window = V.foldl' increment M.empty inVector
        where
        increment freq key = M.insertWith (+) key 1 freq

    sizeIsEven = even (V.length inVector)
    offset = midpointIndex - keyIndex
    midpointIndex = (V.length inVector `div` 2) + 1

    -- break out from a fold over a @Map k v@ ??
    (_, midpoint, keyIndex) = M.foldlWithKey' go (False, -1, 0) window
        where
        go (break, key, count) k v
            | break = (break, key, count)
            | count + v < midpointIndex = (False, k, count + v)
            | otherwise = (True, k, count)


-- | determine where the midpoint goes
--   after one element arrived and another
--   element left the window
--
whichWay :: Int -> Int -> Int -> Direction
whichWay midpoint leaving arriving
    | leaving == arriving = STAY

    --  2  3  4
    --  -  +  |
    --
    --   [1,3,3,4,5,6,7]
    --          |
    --   [1,2,3,4,5,6,7]
    | arriving < midpoint && leaving < midpoint = STAY

    --        4   5   9
    --        |   -   +
    --
    --   [1,2,3,4,6,7,9]
    --          |
    --   [1,2,3,4,5,6,7]
    | arriving > midpoint && leaving > midpoint = STAY

    --    3  4   7 
    --    +  |   -
    --
    --   [1,2,3,3,4,5,6]
    --          |
    --     [1,2,3,4,5,6,7]
    | arriving < midpoint && midpoint < leaving = LEFT

    --    1  4
    --    +  -
    --
    --   [1,1,2,3,5,6,7]
    --          |
    --     [1,2,3,4,5,6,7]
    | leaving == midpoint && arriving < midpoint = LEFT

    --    2  4    6
    --    -  |    +
    --
    --     [1,3,4,5,6,6,7]
    --            |
    --   [1,2,3,4,5,6,7]
    | leaving < midpoint && midpoint < arriving = RIGHT

    --       4    8
    --       -    +
    --
    --     [1,2,3,5,6,7,8]
    --            |
    --   [1,2,3,4,5,6,7]
    | leaving == midpoint && midpoint < arriving = RIGHT

    --    2  4
    --    -  +
    --
    --   [1,3,4,4,5,6,7]
    --          |
    --   [1,2,3,4,5,6,7]
    | arriving == midpoint && leaving < midpoint = STAY

    --       4   6
    --       +   -
    --
    --   [1,2,3,4,4,5,7]
    --          |
    --   [1,2,3,4,5,6,7]
    | arriving == midpoint && midpoint < leaving = STAY


-- | calculate new midpoint from current state and
--   the newly arrived element into the window
--   the "reference" to the midpoint on the updated state is invalid
--   so the current state must be used instead
--
-- assumptions
--  midpoint was deleted
--  => midpoint must move and cannot stay
--
calculateMidpoint :: MovingMedian -> Int -> Direction -> (Int, Int)
calculateMidpoint currentState arriving direction = (updatedMidpoint, updatedOffset)
    where
    (window, _, midpoint, _) = flatten currentState

    (updatedMidpoint, updatedOffset)
      | direction == RIGHT && next < arriving = (next, 1)
      | direction == RIGHT && next == arriving = (next, 1)
      | direction == RIGHT && arriving < next = (arriving, 1)        -- && midpoint < arriving
      | direction == LEFT && arriving < previous = (previous, previousCount)
      | direction == LEFT && arriving == previous = (previous, previousCount+1)
      | direction == LEFT && previous < arriving = (arriving, 1)   -- && arriving < midpoint
      where
        midpointIndex = M.findIndex midpoint window
        (previous, previousCount) = M.elemAt (midpointIndex - 1) window
        (next, nextCount) = M.elemAt (midpointIndex + 1) window


-- | adjust the current midpoint to the updated window
adjustMidpoint :: Map Int Int -> (Int, Int) -> Direction -> (Int, Int)
adjustMidpoint updatedWindow (currentMidpoint, currentOffset) direction = (updatedMidpoint, updatedOffset)
    where
    (updatedMidpoint, updatedOffset)
      | direction == STAY = (currentMidpoint, currentOffset)
      | direction == RIGHT && currentOffset < midpointCount = (currentMidpoint, currentOffset + 1)
      | direction == RIGHT && currentOffset >= midpointCount = (next, 1)
      | direction == LEFT && currentOffset > 1 = (currentMidpoint, currentOffset - 1)
      | direction == LEFT && currentOffset == 1 = (previous, previousCount)
      where
        midpointCount = updatedWindow ! currentMidpoint
        midpointIndex = M.findIndex currentMidpoint updatedWindow
        (previous, previousCount) = M.elemAt (midpointIndex - 1) updatedWindow
        (next, _) = M.elemAt (midpointIndex + 1) updatedWindow


-- | recalculate the median after one value got deleted (leaving) from and
--   another value got inserted (arriving) into the window
--
-- nasty edge case : midpoint got deleted
--
-- (1) do not delete midpoint but leave it with count == 0
--  iterate forward until count of key > 0 to find next
--  as above but iterate backward to find previous
--
-- (2) midpoint gets deleted
--  cannot find previous and next without having the index of midpoint
--  previous window must be used, meaning the one before the delete and insert got applied
--
recalculate :: MovingMedian -> Int -> Int -> MovingMedian
recalculate currentState leaving arriving = MovingMedian updatedWindow sizeIsEven updatedMidpoint updatedOffset
    where
    (window, sizeIsEven, midpoint, offset) = flatten currentState

    updatedWindow = M.update decrement leaving $ M.insertWith (+) arriving 1 window
        where
        decrement x = if x > 1 then Just (x-1)
                      else Nothing     -- key will be deleted

    (updatedMidpoint, updatedOffset) =
        case M.lookup midpoint updatedWindow of
            Just _  -> adjustTo direction
            Nothing -> calculateFrom arriving direction
        where
        direction = whichWay midpoint leaving arriving
        adjustTo = adjustMidpoint updatedWindow (midpoint, offset)
        calculateFrom = calculateMidpoint currentState


median :: MovingMedian -> Int
median state
        | sizeIsEven && offset > 1 = 2 * midpoint
        | sizeIsEven && offset == 1 = previous + midpoint
        | otherwise = 2 * midpoint
        where
        (window, sizeIsEven, midpoint, offset) = flatten state
        midpointIndex = M.findIndex midpoint window
        (previous, _) = M.elemAt (midpointIndex - 1) window


testWhichWay :: [Bool]
testWhichWay = zipWith (==) have expected
    where
    expected = map snd tests
    have = map (func . fst) tests
    func = uncurry3 whichWay

    tests = [
        ((4, 2, 3), STAY),
        ((4, 5, 9), STAY),
        ((4, 7, 3), LEFT),
        ((4, 4, 1), LEFT),
        ((4, 2, 6), RIGHT),
        ((4, 4, 8), RIGHT),
        ((4, 2, 4), STAY),
        ((4, 6, 4), STAY)
        ]


testAdjustMidpoint :: [Bool]
testAdjustMidpoint = zipWith (==) have expected
    where
    expected = map snd tests
    have = map (func . fst) tests
    func = uncurry3 adjustMidpoint
    tests = [
        ((M.fromList [(3,1),(4,2),(7,1)], (4,2), STAY), (4,2)),
        ((M.fromList [(3,1),(4,3),(7,1)], (4,2), RIGHT), (4,3)),
        ((M.fromList [(3,1),(4,2),(7,1)], (4,2), RIGHT), (7,1)),
        ((M.fromList [(3,1),(4,2),(7,1)], (4,2), LEFT), (4,1)),
        ((M.fromList [(3,2),(4,2),(7,1)], (4,1), LEFT), (3,2))
        ]


testRecalculate :: [Bool]
testRecalculate = map run tests
    where
    run (beginState, changes, expected) = (== expected) . mmMidpoint $ L.foldl' apply beginState changes
        where
        apply state (del, ins) = recalculate state del ins

    setupState = MovingMedian (M.fromList [(1,3),(3,1),(4,2),(7,1),(8,3)]) True 4 2
    tests = [
        -- stay, stay, left, left
        (setupState, [(1,3),(8,7),(8,3),(7,1)], 3),
        -- right, right
        (setupState, [(1,8),(1,8)], 8),
        -- left, right, right
        (setupState, [(7,1),(1,9),(1,9)], 8),
        -- right, stay
        (setupState, [(1,9),(7,9)], 8)
        ]


testFromVector :: [Bool]
testFromVector = zipWith (==) have expected
    where
    expected = map snd tests
    have = map ((mmMidpoint &&& mmOffset) . fromVector . fst) tests 
    tests = [
            (V.fromList [1,1,1,3,3,3], (3,1)),
            (V.fromList [1,1,1,3,3], (1,3)),
            (V.fromList [1,2,3,4,5,6], (4,1)),
            (V.fromList [1,2,3,4,5], (3,1))
            ]


runMovingMedianTests :: IO ()
runMovingMedianTests = do
    print testFromVector
    print testWhichWay
    print testAdjustMidpoint
    print testRecalculate


-- | special case of windoSize is 1
-- pairwise comparison
countFor1 :: Vector Int -> Int
countFor1 expenses = fst . V.foldl' fraud initial $ V.tail expenses
    where
    initial = (0, V.head expenses)
    fraud (counter, previous) expense
        | expense >= 2 * previous = (counter+1, expense)
        | otherwise = (counter, expense)


-- | reduce number of sorts
countMovingMedian :: Vector Int -> Int -> Int
countMovingMedian expenses windowSize =
    snd $ L.foldl' fraud (EmptyMovingMedian, 0) [0 .. (n - 1 - windowSize)]
    where
    n = V.length expenses
    fraud :: (MovingMedian, Int) -> Int -> (MovingMedian, Int)
    fraud (state, counter) i
        | expense >= threshold = (nextState, counter+1)
        | otherwise = (nextState, counter)
        where
        nextState
            | i > 0  = recalculate state (expenses V.! (i-1)) (expenses V.! (i-1 + windowSize))
            | i == 0 = fromVector (V.slice 0 windowSize expenses)
        threshold = median nextState
        expense = expenses V.! (i+windowSize)


-- | the median of previous expenses is used
--   to decide whether an expense is fraudulent
countNotifications :: Vector Int -> Int -> Int
countNotifications expenses windowSize
    | windowSize == V.length expenses = 0
    | windowSize == 1 = countFor1 expenses
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


runCountTests :: IO ()
runCountTests = do
    print testCountNotifications


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


solve :: IO ()
solve = do
    readInput >>=
        print . uncurry countNotifications


main :: IO ()
main = do
    runMovingMedianTests
    runCountTests
    -- solve
