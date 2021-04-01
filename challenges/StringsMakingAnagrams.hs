module Main where

import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map (Map)
import System.Environment.Blank
import System.IO

-- |
-- anagrams contain the same exact letters in the same exact frequency
--  bacdc and dcbac are anagrams
-- only operation allowed is delete a character
--
-- remove from both strings the characters they have in common
--  fgabdcc \\ (fgabdcc intersect daabc) == fgc
--  daabc \\ (fgabdcc intersect daabc) == a
-- count the characters in the result
--
-- > makeAnagram "dab" "bac" == 2
makeAnagram :: String -> String -> Int
makeAnagram fstWord sndWord
    | length fstWord < length sndWord = length $ lettersNotShared fstWord sndWord
    | otherwise = length $ lettersNotShared sndWord fstWord

-- |
-- > lettersNotShared "dab" "bac" == "cd"
-- > lettersNotShared "fgabdcc" "daabc" == "fgca"
lettersNotShared :: String -> String -> String
lettersNotShared shortWord longWord = mergeLetters
    where
    mergeLetters = (freqToList notSharedLong) ++ notSharedShort
    (notSharedLong, notSharedShort) = foldl go (frequencies longWord, []) shortWord
    go (searchThis, notFound) letter
        | wasFound = (freqDecr letter searchThis, notFound)
        | otherwise = (searchThis, letter : notFound)
        where
        wasFound = Map.member letter searchThis

-- | build frequency table of the characters in a string
-- > frequencies "halla" == Map.fromList [('a',2),('h',1),('l',2)]
frequencies :: String -> Map Char Int
frequencies str = Map.fromListWith (+) $ zip str (repeat 1)

-- | decrement the count of a key in a frequency table
-- > freqDecr 'a' Map.fromList [('a',2),('h',1)] == Map.fromList [('a',1),('h',1)]
-- > freqDecr 'a' Map.fromList [('a',1),('h',1)] == Map.fromList [('h',1)]
freqDecr :: Char -> Map Char Int -> Map Char Int
freqDecr k freqTable = Map.update f k freqTable
    where
    f x = if x > 1 then Just (x-1) else Nothing

-- | characters in their frequencies combined into a string
-- > freqToList Map.fromList [('a',2),('h',1),('l',2)] == "aahll"
freqToList :: Map Char Int -> String
freqToList freq = Map.foldrWithKey go [] freq
    where
    go k v acc = (take v $ repeat k) ++ acc

-- | convert list with two elements into a pair
pairify :: [a] -> (a,a)
pairify [x,y] = (x,y)

-- | parse string from stdin
input :: IO (String, String)
input = do
    getContents >>= return . pairify . words

main :: IO ()
main = do
    outFilePath <- getEnvDefault "OUTPUT_PATH" "/dev/stdout"
    withFile outFilePath WriteMode (\fptr -> do
        input >>=
            hPutStrLn fptr . show . uncurry makeAnagram
        )
