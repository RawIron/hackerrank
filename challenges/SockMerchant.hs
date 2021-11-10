module Main where

import Data.List as List
import Data.Map as Map
import System.Environment.Blank
import System.IO


-- | build frequency table of socks
-- > frequencies [1,2,1,3,4,4,1] == Map.fromList [(4,2),(3,1),(2,1),(1,3)]
frequencies :: [Int] -> Map Int Int
frequencies socks = Map.fromListWith (+) $ zip socks (repeat 1)

-- | count pairs of socks
-- > freqToCount Map.fromList [(4,2),(3,1),(2,1),(1,3)] == 2
freqToCount :: Map Int Int -> Int
freqToCount freq = Map.foldrWithKey go 0 freq
    where
    go k v acc = acc + (quot v 2)

-- | count the pairs of socks
-- > countPairs [1,2,1,3,4,4,1] == 2
-- > countPairs [1,2,3] == 0
countPairs :: [Int] -> Int
countPairs socks = freqToCount . frequencies $ socks


-- | parse string from stdin
input :: IO [Int]
input = do
    getContents >>= return . parse . List.map read . words
    where
    parse numbers = (tail numbers)

main :: IO ()
main = do
    outFilePath <- getEnvDefault "OUTPUT_PATH" "/dev/stdout"
    withFile outFilePath WriteMode (\fptr -> do
        input >>=
            hPutStrLn fptr . show . countPairs
        )
