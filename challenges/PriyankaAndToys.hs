module Main where

import Data.List as L

-- | build groups where each element in group
-- | is within span of the smallest element in group
-- > groupWithinSpan 2 [3,4,1,2,5] == [[5,4],[3,2,1]]
groupWithinSpan :: Int -> [Int] -> [[Int]]
groupWithinSpan span numbers = snd $ foldl go (minNumber, [[minNumber]]) $ tail sortedNumbers
    where
    sortedNumbers = L.sort numbers
    minNumber = head sortedNumbers
    go (groupSmallest, (g:gs)) number
        | groupSmallest + span >= number = (groupSmallest, ((number:g):gs))
        | otherwise = (number, ([number]:g:gs))

-- | parse input from stdin
input :: IO [Int]
input = do
    getContents >>= return . map read . tail . words

main :: IO()
main = do
    input >>= putStrLn . show . length . groupWithinSpan 4
