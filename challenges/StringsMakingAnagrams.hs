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
-- > makeAnagram "dab" "bac" == 2
makeAnagram :: String -> String -> Int
makeAnagram fstWord sndWord
    | length fstWord < length sndWord = length $ lettersNotShared fstWord sndWord
    | otherwise = length $ lettersNotShared sndWord fstWord

-- |
lettersNotShared :: String -> String -> String
lettersNotShared shortWord longWord = mergeLetters
    where
    mergeLetters = (freqToList deleteFromLong) ++ deleteFromShort
    (deleteFromLong, deleteFromShort) = foldl go (frequencies longWord, []) shortWord
    go (searchThis, notFound) letter
        | wasFound = (decrOrDelete, notFound)
        | otherwise = (searchThis, letter : notFound)
        where
        wasFound = Map.member letter searchThis
        decrOrDelete = freqDecr letter searchThis

-- |
frequencies :: String -> Map Char Int
frequencies str = Map.fromListWith (+) $ zip str (repeat 1)

freqDecr :: Char -> Map Char Int -> Map Char Int
freqDecr k freqTable = Map.update f k freqTable
    where
    f x = if x > 1 then Just (x-1) else Nothing
    
freqToList :: Map Char Int -> String
freqToList freq = Map.foldrWithKey go [] freq
    where
    go k v acc = (take v $ repeat k) ++ acc

-- |
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
