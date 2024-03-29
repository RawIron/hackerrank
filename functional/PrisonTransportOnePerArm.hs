module Main where

import Data.Set (Set)
import qualified Data.Set as Set
import Control.Arrow ((&&&))

-- | extract all elements from set which are not in any pair
-- > filterNotInAnyPair (Set.fromList [1,2,7]) [(1,2),(1,3),(2,4)] == Set.fromList [7]
filterNotInAnyPair :: (Ord a) => Set a -> [(a,a)] -> Set a
filterNotInAnyPair members pairs = members Set.\\ seenInPairs
    where
    seenInPairs = foldl go Set.empty pairs
        where
        go seen (x,y)
            | xyWereSeen = ((Set.insert y . Set.insert x) $ seen)
            | xWasSeen = (Set.insert x seen)
            | yWasSeen = (Set.insert y seen)
            | otherwise = seen
            where
            xyWereSeen = (Set.member x members) && (Set.member y members)
            xWasSeen = Set.member x members
            yWasSeen = Set.member y members

-- | extract all pairs which have both of their elements in the Set
-- > filterBothAreMember (Set.fromList [1,2]) [(1,2),(1,3),(2,4)] == [(1,2)]
filterBothAreMember :: (Ord a) => Set a -> [(a,a)] -> [(a,a)]
filterBothAreMember members pairs = filter (go members) pairs
    where
    go members (x,y) = (Set.member x members) && (Set.member y members)

-- | extract all pairs which have none of their elements in the Set
-- > filterNoneIsMember (Set.fromList [1,2]) [(1,2),(1,3),(4,5)] == [(4,5)]
filterNoneIsMember :: (Ord a) => Set a -> [(a,a)] -> [(a,a)]
filterNoneIsMember members pairs = filter (go members) pairs
    where
    go members (x,y) = not ((Set.member x members) || (Set.member y members))

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
-- > duplicatesP [(9,10),(10,11),(11,12)] == Set.fromList [10,11]
duplicatesP :: (Ord a) => [(a,a)] -> Set a
duplicatesP xs = fst $ foldl go (Set.empty, Set.empty) xs
    where
    go (repeated, seen) (x,y)
        | xyWereSeen = ((Set.insert y . Set.insert x) $ repeated, seen)
        | xWasSeen = (Set.insert x repeated, Set.insert y seen)
        | yWasSeen = (Set.insert y repeated, Set.insert x seen)
        | otherwise = (repeated, (Set.insert x . Set.insert y) $ seen)
        where
        xyWereSeen = (Set.member x seen) && (Set.member y seen)
        xWasSeen = Set.member x seen
        yWasSeen = Set.member y seen

-- | calculate how many groups of each size
-- 
-- group of size 2 is a single pair (7,8)
--  in case all groups are of size 2 [(7,8),(20,21),(22,23)]
--  the list [7,8,20,21,22,23] is a set
--
-- group of size 3 are two pairs (2,1),(1,3)
--  the list [2,1,1,3] has 1 as a duplicated value
--  a connection of two pairs is a duplication of one value in the list
--    2-1     1-3    4-5     5-6
--      +- 1 -+        +- 5 -+
--
-- group of size 4 are 3 pairs (9,10),(10,11),(11,12)
--  the list [9,10,10,11,11,12] has 10,11 as duplicated values
--    9-10      10-11      11-12
--       +- 10 -+   +- 11 -+
--              10-11
--
-- group of size 6 are 5 pairs (18,19),(19,20),(20,21),(21,22),(22,23)
--    18-19      19-20      20-21      21-22      22-23
--        +- 19 -+   +- 20 -+   +- 21 -+   +- 22 -+
--               19-20      20-21      21-22
--                   +- 20 -+   +- 21 -+
--                          20-21
--
-- 2-1-3, 4-5-6, 7-8
-- > reportGroups [(2,1),(1,3),(4,5),(5,6),(7,8)] == [1,2]
-- 2-1-3, 4-5-6, 7-8, 9-10-11-12
-- > reportGroups [(2,1),(1,3),(4,5),(5,6),(7,8),(9,10),(10,11),(11,12)] == [1,2,1,0]
-- 9-10-11-12
-- > reportGroups [(9,10),(10,11),(11,12)] == [0,0,1,0]
-- 13-14-15-16-17
-- > reportGroups [(13,14),(14,15),(15,16),(16,17)] == [0,0,0,1]
-- 18-19-20-21-22-23
-- > reportGroups [(18,19),(19,20),(20,21),(21,22),(22,23)] == [0,0,0,0,1,0]
reportGroups :: [(Int,Int)] -> [Int]
reportGroups pairs = reverse $ go [] pairs
    where
    go groups [] = groups
    go groups pairs = go groupsNext pairsNext
        where
        repeats = duplicatesP pairs
        groupsP = length (filterNoneIsMember repeats pairs) : groups
        pairsNext = filterBothAreMember repeats pairs
        groupsNext = Set.size (filterNotInAnyPair repeats pairsNext) : groupsP

-- | transport groups of inmates at lowest bus cost to a new location
-- bus cost
--  they make it necessary to find all groups with their size
--  one group per bus
--
-- groups
--  have to determine the number of groups and their sizes
--  for example one group of size 4:
--      3-1-2-4 == [(3,1),(1,2),(2,4)]
--  inmates can have one handcuff per arm
--  all inmates have both of their arms
--  inmates are chained in a line so they can get on the bus (no cycles)
--  invalid inputs
--      [(1,2),(1,3),(1,4)] more than 1 handcuff on one arm
--      [(1,2),(2,1)] cycle, not chained in a line
--
-- 2-1-3, 4-5-6, 7-8
-- > transportInmatesCost 8 [(2,1),(1,3),(4,5),(5,6),(7,8)] == 6
-- 18-19-20-21-22-23
-- > transportInmatesCost 6 [(18,19),(19,20),(20,21),(21,22),(22,23)] == 3
-- 2-1-3, 4-5-6, 7-8, 9-10-11-12
-- > transportInmatesCost 12 [(2,1),(1,3),(4,5),(5,6),(7,8),(9,10),(10,11),(11,12)] == 8
transportInmatesCost :: Int -> [(Int,Int)] -> Int
transportInmatesCost n pairs = sum $ zipWith (*) (singleInmates : groups) busCost
    where
    groups = reportGroups pairs
    chainedInmates = sum $ zipWith (*) groups [2..]
    singleInmates = n - chainedInmates
    busCost = map (ceiling . sqrt . fromIntegral) [1..]


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
-- identical to (drop 2)
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
    input >>= putStrLn . show . uncurry transportInmatesCost
