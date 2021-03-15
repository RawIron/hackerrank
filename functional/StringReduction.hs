module Main where

import Data.Set (Set)
import qualified Data.Set as Set

useFold :: String -> String
useFold word = (reverse . snd) $ foldl go (Set.empty, []) word where
    go (seen, reduced) w
        | (Set.member w seen) = (seen, reduced)
        | otherwise = ((Set.insert w seen), (w:reduced))

useRecursion :: String -> String
useRecursion word = go Set.empty [] word where
    go _ reduced [] = reverse reduced
    go seen reduced (w:ws)
        | (Set.member w seen) = go seen reduced ws
        | otherwise = go (Set.insert w seen) (w:reduced) ws

-- | keep only the first occurrence of a char in a string
-- >>> keepUniqueChars occurence
-- ocuren
keepUniqueChars :: String -> String
keepUniqueChars = useFold

main :: IO ()
main = do
    getContents >>= putStrLn . keepUniqueChars
