module Main where

import Data.Set (Set)
import qualified Data.Set as Set

-- | keep only the first occurrence of a char in a string
-- >>> keepUniqueChars occurence
-- ocuren
keepUniqueChars :: String -> String
keepUniqueChars word = go Set.empty [] word where
    go _ reduced [] = reverse reduced
    go seen reduced (w:ws)
        | (Set.member w seen) = go seen reduced ws
        | otherwise = go (Set.insert w seen) (w:reduced) ws

main :: IO ()
main = do
    getContents >>= putStrLn . keepUniqueChars
