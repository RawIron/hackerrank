module Main where

import Data.Set (Set)
import qualified Data.Set as Set
import Control.Arrow ((&&&))

-- | extract all pairs which have both of their elements in the Set
-- > filterBothAreMember Set.fromList [1,2] [(1,2),(1,3),(2,4)] == [(1,2)]
filterBothAreMember :: (Ord a) => Set a -> [(a,a)] -> [(a,a)]
filterBothAreMember members pairs = filter (go members) pairs
    where
    go members (x,y) = (Set.member x members) && (Set.member y members)

-- | extract all elements which occur more than once
-- > duplicates [1,2,1,2,2,3,4] == Set.fromList [1,2]
duplicates :: (Ord a) => [a] -> Set a
duplicates xs = fst $ foldl go (Set.empty, Set.empty) xs
    where
    go (repeated, seen) x
        | Set.member x seen = (Set.insert x repeated, seen)
        | otherwise = (repeated, Set.insert x seen)

-- | extract all elements which occur more than once
-- > duplicatesP [(1,2),(1,4),(2,3),(4,3)] == Set.fromList [1,2,3,4]
duplicatesP :: (Ord a) => [(a,a)] -> Set a
duplicatesP xs = fst $ foldl go (Set.empty, Set.empty) xs
    where
    go (repeated, seen) (x,y)
        | xyWereSeen = ((Set.insert y . Set.insert x) $ repeated, seen)
        | xWasSeen = (Set.insert x repeated, seen)
        | yWasSeen = (Set.insert y repeated, seen)
        | otherwise = (repeated, (Set.insert x . Set.insert y) $ seen)
        where
        xyWereSeen = (Set.member x seen) && (Set.member y seen)
        xWasSeen = Set.member x seen
        yWasSeen = Set.member y seen

-- | transport groups of inmates at lowest bus cost to a new location
-- bus cost
--  it makes it necessary to find all groups with their size
--  a^2 + b^2 < (a + b)^2 makes allocation of groups to buses trivial
--  put each group in a separate bus
--
-- number of groups and their sizes
-- groups of size > 1
-- [(3,1),(1,2),(2,4)] 3-1-2-4
transportInmatesCost :: Int -> [(Int,Int)] -> Int
transportInmatesCost n pairs =  n - countGroups - sizeGroups
    where
    repeats = duplicatesP pairs
    countGroups = length repeats
    sizeGroups = length $ filterBothAreMember repeats pairs

solve :: Int -> [(Int, Int)] -> Int
solve n pairs = transportInmatesCost n pairs


-- | convert a list to a list of pairs
-- identical to sliceVertPair implementation
-- > slicePairs [2,3,4,5] == [(2,3),(4,5)]
slicePairs :: [a] -> [(a, a)]
slicePairs [] = []
slicePairs (x:[]) = []
slicePairs (x:y:xs) = (x,y) : slicePairs xs

-- | convert a list of pairs to a list
-- > concatPairs [(2,3),(4,5)] == [2,3,4,5]
concatPairs :: [(a,a)] -> [a]
concatPairs [] = []
concatPairs ((x,y):xs) = x:y : concatPairs xs

-- | apply two functions on the same value
-- identical to (f &&& g)
-- > ((+1) &&& (*3)) 9 == (10, 27)
-- > forkPair (+1) (*3) 9 == (10, 27)
forkPair :: (a -> b) -> (a -> c) -> a -> (b, c)
forkPair f g x = (f x, g x)

-- | apply three functions on the same value
-- > forkTriple (+1) (*3) (^2) 9 == (10, 27, 81)
-- > ((+1) &&& (*3) &&& (^2)) 9 == (10, (27, 81))
forkTriple :: (a -> b) -> (a -> c) -> (a -> d) -> a -> (b, c, d)
forkTriple f g h x = (f x, g x, h x)

-- | use values of a triple as function arguments
-- > uncurry3 (\x y z -> x + y + z) (1,2,3) == 6
uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f ~(a, b, c) = f a b c

-- | take two heads off
tail2 :: [a] -> [a]
tail2 = tail . tail

-- | parse the input as one string
-- reading each line separately is simpler
-- done this way to practice function composition
input :: IO (Int, [(Int, Int)])
input = 
    getContents >>=
        return . (forkPair head (slicePairs . tail2)) . map read . words

main :: IO ()
main = do
    input >>= putStrLn . show . uncurry solve
