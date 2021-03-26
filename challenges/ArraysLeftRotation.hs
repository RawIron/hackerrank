module Main where

import qualified Data.List as List
import System.Environment
import System.IO

-- | k-times left rotate of a list
--
-- index the elements in the list with [0..(length list)-1]
-- new index = mod ((length list) - rotation + old index) (length list)
-- [3,4,6,5] 3 , 4 has the index 1
-- the new index of 4 is: 2 = mod (4 - 3 + 1) 4
--
-- > rotateLeft [3,4,6,5] 3 == [5,3,4,6]
rotateLeft :: [Int] -> Int -> [Int]
rotateLeft numbers rotations = leftOfHeadIndex ++ headIndexToEnd
    where
    n = length numbers
    headIndex = mod (n - rotations) n
    leftOfHeadIndex = reverse . take headIndex . reverse $ numbers
    headIndexToEnd = take (n-headIndex) numbers


-- | take the head and tail of a list and make it a pair
-- > pairHeadTail [2,4,6] == (2,[4,6])
pairHeadTail :: [a] -> (a, [a])
pairHeadTail (x:xs) = (x, xs)

-- | reverse function for pairs
-- > reversePair (4,6) == (6,4)
reversePair :: (a, b) -> (b, a)
reversePair (x, xs) = (xs, x)

-- | parse string from stdin
input :: IO ([Int], Int)
input = do
    getContents >>=
        return . reversePair . pairHeadTail . List.map read . tail . words

main :: IO ()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    input >>=
        hPutStrLn fptr . List.intercalate " " . List.map show . uncurry rotateLeft

    hFlush fptr
    hClose fptr
