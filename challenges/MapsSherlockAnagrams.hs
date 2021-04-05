module Main where

import Control.Monad
import qualified Data.List as List
import System.Environment.Blank
import System.IO

-- | count the substrings which are anagrams of each other
--
-- a substring is a slice of a string, for example "hallow"[3:5] == "lo"
-- Two strings are anagrams of each other
-- if the letters of one string can be rearranged to form the other string.
--
-- > countAnagrams "abbad" == 4
countAnagrams :: String -> Integer
countAnagrams word = sum $ countSubstrAnagrams word

-- | count the number of ways pairs can be arranged for every group of substrings
--
-- group with 3 substrings indexed 1..3 -> 1,2 1,3 2,3
-- > countSubstrAnagrams "abbad" == [2,1,1,0]
countSubstrAnagrams :: String -> [Integer]
countSubstrAnagrams word  = map (count . groupSubstr word) [1..n]
    where
    n = (length word) - 1
    count = sum . map countPairs . map (toInteger . length)

-- | all substrings of length N sorted and grouped
--   only groups with cardinality > 1 are returned
-- in a group are all substrings with the same characters
-- and same frequency for every character
--
-- substr of length 2 for abbad -> ab, bb, ba, ad
-- > groupSubstr "abbad" 2 == [["ab","ab"]]
-- > groupSubstr "abbad" 3 == [["abb","abb"]]
groupSubstr :: String -> Int -> [[String]]
groupSubstr word n = (filter ((>1) . length) . List.group . List.sort) $ go [] n word
    where
    go :: [String] -> Int -> String -> [String]
    go acc _ [] = acc
    go acc n str
        | n > length str = acc
        | otherwise = go ((List.sort $ take n str) : acc) n (tail str)

-- | combinatorics - combinations (order does not matter) without repetition
--   n choose 2
-- > countPairs 4 == 6
countPairs :: Integer -> Integer
countPairs n = div (fact n) (2 * fact (n-2))

-- | factorial of n
fact :: Integer -> Integer
fact 0 = 1
fact n = product [1..n]

main :: IO()
main = do
    outFilePath <- getEnvDefault "OUTPUT_PATH" "/dev/stdout"
    withFile outFilePath WriteMode (\fptr -> do  
        t <- readLn :: IO Int
        forM_ [1..t] $ \t_itr -> do
            getLine >>= hPutStrLn fptr . show . countAnagrams
        )
