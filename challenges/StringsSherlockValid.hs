module Main where

import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map (Map)
import System.Environment.Blank
import System.IO

-- | is this a string which has all characters with identical frequencies
--   or can only 1 character be removed and the remaining string is as above
--
-- one group:
--  all frequencies are identical
-- two groups:
--  one group must have count 1
--    frequency-1 of the group with count 1 == frequency of the other group
--    frequency is 1 of the group with count 1
-- more than two groups:
--  word is invalid
--
-- > isValid "aabb" == True
-- > isValid "abbabac" == True
-- > isValid "aabbc" == True
-- > isValid "aacbcbc" == True
isValid :: String -> Bool
isValid word = case (groupFreq $ frequencies word) of
    (xs:[]) -> True
    ([1]:xs:[]) -> True
    (xs:(x:[]):[]) -> (head xs) == (x-1)
    otherwise -> False

-- | groups of frequencies sorted ascending
-- > groupFreq $ frequencies "aabbc" == [[1],[2,2]]
-- > groupFreq $ frequencies "aacbcbc" == [[2,2],[3]]
groupFreq :: Map Char Int -> [[Int]]
groupFreq = List.group . List.sort . Map.elems

-- | build frequency table of the characters in a string
-- > frequencies "halla" == Map.fromList [('a',2),('h',1),('l',2)]
frequencies :: String -> Map Char Int
frequencies str = Map.fromListWith (+) $ zip str (repeat 1)

-- | hackerrank output
showYesNo :: Bool -> String
showYesNo isTrue
    | isTrue = "YES"
    | otherwise = "NO"

-- | parse string from stdin
input :: IO String
input = do
    getContents >>= return . head . words

main :: IO ()
main = do
    outFilePath <- getEnvDefault "OUTPUT_PATH" "/dev/stdout"
    withFile outFilePath WriteMode (\fptr -> do
        input >>=
            hPutStrLn fptr . showYesNo . isValid
        )
