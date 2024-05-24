module Main where

import Control.Arrow ( (&&&) )
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
        mmWindow :: Map Int Int,
        mmMidpoint :: Int,
        mmOffset :: Int
        }


flatten :: MovingMedian -> (Map Int Int, Int, Int)
flatten state = (window, midpoint, offset)
    where
    window = mmWindow state
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
fromVector inVector = MovingMedian window midpoint offset
    where
    window = V.foldl' increment M.empty inVector
        where
        increment freq key = M.insertWith (+) key 1 freq

    offset = midpointIndex - keyIndex
    midpointIndex = (V.length inVector `div` 2) + 1

    -- break out from a fold over a @Map k v@ ??
    (_, midpoint, keyIndex) = M.foldlWithKey' go (False, -1, 0) window
        where
        go (break, key, count) k v
            | break = (break, key, count)
            | count + v < midpointIndex = (False, k, count + v)
            | otherwise = (True, k, count)


-- | determine where the median goes
--   after a one element arrived and another
--   element left the window
--
-- stays ==  0
-- right ==  1
-- left  == -1
--
whichWay :: Int -> Int -> Int -> Int
whichWay midPointValue deleteValue insertValue
    | deleteValue == insertValue = 0

    --  2  3  4
    --  -  +  |
    --
    --   [1,3,3,4,5,6,7]
    --          |
    --   [1,2,3,4,5,6,7]
    | insertValue < midPointValue && deleteValue < midPointValue = 0

    --        4   5   9
    --        |   -   +
    --
    --   [1,2,3,4,6,7,9]
    --          |
    --   [1,2,3,4,5,6,7]
    | insertValue > midPointValue && deleteValue > midPointValue = 0

    --    3  4   7 
    --    +  |   -
    --
    --   [1,2,3,3,4,5,6]
    --          |
    --     [1,2,3,4,5,6,7]
    | insertValue < midPointValue && midPointValue < deleteValue = -1

    --    1  4
    --    +  -
    --
    --   [1,1,2,3,5,6,7]
    --          |
    --     [1,2,3,4,5,6,7]
    | deleteValue == midPointValue && insertValue < midPointValue = -1

    --    2  4    6
    --    -  |    +
    --
    --     [1,3,4,5,6,6,7]
    --            |
    --   [1,2,3,4,5,6,7]
    | deleteValue < midPointValue && midPointValue < insertValue = 1

    --       4    8
    --       -    +
    --
    --     [1,2,3,5,6,7,8]
    --            |
    --   [1,2,3,4,5,6,7]
    | deleteValue == midPointValue && midPointValue < insertValue = 1

    --    2  4
    --    -  +
    --
    --   [1,3,4,4,5,6,7]
    --          |
    --   [1,2,3,4,5,6,7]
    | insertValue == midPointValue && deleteValue < midPointValue = 0

    --       4   6
    --       +   -
    --
    --   [1,2,3,4,4,5,7]
    --          |
    --   [1,2,3,4,5,6,7]
    | insertValue == midPointValue && midPointValue < deleteValue = 0


-- | calculate new midpoint from current state and
--   the newly arrived element into the window
--   the "reference" to the midpoint on the updated state is invalid
--   so the current state must be used instead
--
-- assumptions
--  midpoint was deleted
--  => midpoint must move and cannot stay
--
calculateMidpoint :: MovingMedian -> Int -> Int -> (Int, Int)
calculateMidpoint currentState insertValue direction = (updatedKey, updatedOffset)
    where
    (mWindow, mKey, mOffset) = flatten currentState

    (updatedKey, updatedOffset)
      | direction == 1 && mKeyNextValue < insertValue = (mKeyNext, 1)
      | direction == 1 && mKeyNextValue == insertValue = (mKeyNext, 1)
      | direction == 1 && insertValue < mKeyNextValue = (insertValue, 1)        -- && midpointValue < insertValue
      | direction == -1 && insertValue < mKeyPreviousValue = (mKeyPrevious, mKeyPreviousValue)
      | direction == -1 && insertValue == mKeyPreviousValue = (mKeyPrevious, mKeyPreviousValue+1)
      | direction == -1 && mKeyPreviousValue < insertValue = (insertValue, 1)   -- && insertValue < midpointValue
      where
        keyCount = mWindow ! mKey
        mKeyIndex = M.findIndex mKey mWindow
        (mKeyPrevious, mKeyPreviousValue) = M.elemAt (mKeyIndex - 1) mWindow
        (mKeyNext, mKeyNextValue) = M.elemAt (mKeyIndex + 1) mWindow


-- | adjust the current midpoint to the updated window
adjustMidpoint :: Map Int Int -> (Int, Int) -> Int -> (Int, Int)
adjustMidpoint updatedWindow (currentKey, currentOffset) direction = (updatedKey, updatedOffset)
    where
    (updatedKey, updatedOffset)
      | direction == 0 = (currentKey, currentOffset)
      | direction == 1 && currentOffset < keyCount = (currentKey, currentOffset + 1)
      | direction == 1 && currentOffset == keyCount = (mKeyNext, 1)
      | direction == -1 && currentOffset > 1 = (currentKey, currentOffset - 1)
      | direction == -1 && currentOffset == 1 = (mKeyPrevious, mKeyPreviousValue)
      where
        keyCount = updatedWindow ! currentKey
        mKeyIndex = M.findIndex currentKey updatedWindow
        (mKeyPrevious, mKeyPreviousValue) = M.elemAt (mKeyIndex - 1) updatedWindow
        (mKeyNext, _) = M.elemAt (mKeyIndex + 1) updatedWindow


-- | recalculate the median after one value got deleted from and
--   another value got inserted into the window
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
recalculate currentState deleteValue insertValue = MovingMedian updatedWindow updatedKey updatedOffset
    where
    (window, midpointValue, midpointOffset) = flatten currentState

    updatedWindow = M.update decrement deleteValue $ M.insertWith (+) insertValue 1 window
        where
        decrement x = if x > 1 then Just (x-1)
                      else Nothing     -- key will be deleted

    (updatedKey, updatedOffset) =
        case M.lookup midpointValue updatedWindow of
            Just _  -> adjustTo direction
            Nothing -> calculateFrom insertValue direction
        where
        direction = whichWay midpointValue deleteValue insertValue
        adjustTo = adjustMidpoint updatedWindow (midpointValue, midpointOffset)
        calculateFrom = calculateMidpoint currentState


testWhichWay :: [Bool]
testWhichWay = zipWith (==)  have expected
    where
    expected = map snd tests
    have = map (func . fst) tests
    func = uncurry3 whichWay

    tests = [
        ((4, 2, 3), 0),
        ((4, 5, 9), 0),
        ((4, 7, 3), -1),
        ((4, 4, 1), -1),
        ((4, 2, 6), 1),
        ((4, 4, 8), 1),
        ((4, 2, 4), 0),
        ((4, 6, 4), 0)
        ]


testAdjustMidpoint :: [Bool]
testAdjustMidpoint = zipWith (==)  have expected
    where
    expected = map snd tests
    have = map (func . fst) tests
    func = uncurry3 adjustMidpoint
    tests = [
        ((M.fromList [(3,1),(4,2),(7,1)], (4,2), 0), (4,2)),
        ((M.fromList [(3,1),(4,3),(7,1)], (4,2), 1), (4,3)),
        ((M.fromList [(3,1),(4,2),(7,1)], (4,2), 1), (7,1)),
        ((M.fromList [(3,1),(4,2),(7,1)], (4,2), -1), (4,1)),
        ((M.fromList [(3,2),(4,2),(7,1)], (4,1), -1), (3,2))
        ]


testRecalculate :: [Bool]
testRecalculate = map run tests
    where
    run (initState, changes, expected) = (== expected) . mmMidpoint $ L.foldl' go initState changes
    go state (del, ins) = recalculate state del ins

    setupState = MovingMedian (M.fromList [(1,3),(3,1),(4,2),(7,1),(8,3)]) 4 2
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


runTests :: IO ()
runTests = do
    print testFromVector
    print testWhichWay
    print testAdjustMidpoint
    print testRecalculate


main :: IO ()
main = do
    runTests
