module Main where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.List as List
import Data.Functor ((<&>))
import Control.Arrow ((&&&))
import Control.Monad(foldM)


-- |
runTests :: Eq b => (a -> b) -> [(a, b)] -> [Bool]
runTests f tests = zipWith (==) (map (f . fst) tests) (map snd tests)


runAllTests :: [[Bool]]
runAllTests = List.foldl' (flip (:)) [] [testMemberIn2, testMergeGroups, testReportGroups]


-- | split a list into evenly sized batches
batch :: Ord a => Int -> [a] -> [[a]]
batch batchSize xs = fst . List.foldl' go ([], xs) $ [1 .. (div (length xs) batchSize)+1]
    where
    go (splits, remainder) _ = (flip List.insert splits . fst) &&& snd $ splitAt batchSize remainder

batch10k :: Ord a => [a] -> [[a]]
batch10k = batch 10000

exampleBatch :: Bool
exampleBatch =
    batch 3 [2,4,5,7,8] == [[2,4,5],[7,8]]


-- | extract the set where x is a member of
--
memberInNaive :: (Ord a) => a -> Set(Set a) -> Set a
memberInNaive x groups =
    if Set.null inGroup then Set.empty
    else Set.elemAt 0 inGroup
    where
    inGroup = Set.filter (Set.member x) groups


exampleMemberInNaive :: Bool
exampleMemberInNaive =
    memberInNaive 4 (Set.fromList [Set.fromList [1,2], Set.fromList [3,4,5]]) == Set.fromList [3,4,5]


-- | extract the set where x is a member of
--
memberIn :: (Ord a) => a -> Set(Set a) -> Set a
memberIn x = either id id . foldM go Set.empty
    where
    go xMember group =
        if Set.member x group
        then Left group         -- Left signals early termination
        else Right xMember


-- | extract the sets where x,y is a member of
--
-- search for two in one iteration
-- break out of @foldM@ as soon as both sets were found
memberIn2 :: (Ord a) => (a,a) -> Set(Set a) -> (Set a, Set a)
memberIn2 (x,y) = either id id . foldM go (Set.empty, Set.empty)
    where
    go (xMember, yMember) group =
        case (Set.null xMember, Set.null yMember) of
        (False, False) -> Left (xMember, yMember)       -- Left signals early termination
        _              -> Right (xIn, yIn)              -- keep iterating
        where
        xIn = if Set.member x group then group else xMember
        yIn = if Set.member y group then group else yMember


testMemberIn2 :: [Bool]
testMemberIn2 = runTests (uncurry memberIn2) tests
    where
    tests = [
        (((4,7), Set.fromList [Set.fromList [1,2], Set.fromList [3,4,5]]), (Set.fromList [3,4,5], Set.empty)),
        (((6,1), Set.fromList [Set.fromList [1,2], Set.fromList [3,4,5]]), (Set.empty, Set.fromList [1,2]))
        ]


-- | calculate how many groups of each size
--
-- 2-1-3, 4-5-6, 7-8
-- > reportGroups [(2,1),(1,3),(4,5),(5,6),(7,8)] == [3,3,2]
-- 2-1-3, 4-5-6, 7-8, 9-10-11-12
-- > reportGroups [(2,1),(1,3),(4,5),(5,6),(7,8),(9,10),(10,11),(11,12)] == [3,3,2,4]
-- 9-10-11-12
-- > reportGroups [(9,10),(10,11),(11,12)] == [4]
-- 13-14-15-16-17
-- > reportGroups [(13,14),(14,15),(15,16),(16,17)] == [5]
-- 18-19-20-21-22-23
-- > reportGroups [(18,19),(19,20),(20,21),(21,22),(22,23)] == [6]
reportGroups :: [(Int,Int)] -> [Set Int]
reportGroups = Set.toList . snd . List.foldl' go (Set.empty, Set.empty)
    where
    go (seenBefore, groups) (x,y) =
        case (Set.member x seenBefore, Set.member y seenBefore) of
        (False, False) -> (rememberXY, addNewGroup)
        (False, True) -> (rememberX, insertXtoY)
        (True, False) -> (rememberY, insertYtoX)
        (True, True) -> if xyMemberIn == yxMemberIn then (seenBefore, groups)
                        else (seenBefore, unionXY)
        where
        rememberXY = Set.insert x . Set.insert y $ seenBefore
        rememberX = Set.insert x seenBefore
        rememberY = Set.insert y seenBefore
        addNewGroup = Set.insert (Set.fromList [x,y]) groups
        insertYtoX = Set.insert (Set.insert y xMemberIn) . Set.delete xMemberIn $ groups
        insertXtoY = Set.insert (Set.insert x yMemberIn) . Set.delete yMemberIn $ groups
        unionXY = Set.insert (Set.union xyMemberIn yxMemberIn) . Set.delete yxMemberIn . Set.delete xyMemberIn $ groups
        xMemberIn = memberIn x groups
        yMemberIn = memberIn y groups
        (xyMemberIn, yxMemberIn) = memberIn2 (x,y) groups


testReportGroups :: [Bool]
testReportGroups = runTests (List.sort . reportGroups) tests
    where 
    tests = [
        ([(1,2)], [Set.fromList [1,2]]),
        ([(1,2),(3,4)], [Set.fromList [1,2], Set.fromList [3,4]]),
        ([(2,1),(1,3)], [Set.fromList [2,1,3]]),
        ([(2,1),(1,3),(4,5),(5,6)], [Set.fromList [2,1,3], Set.fromList [4,5,6]]),
        ([(2,1),(1,3),(4,5),(5,6),(7,8),(9,10),(10,11),(11,12)],
            [Set.fromList [2,1,3], Set.fromList [4,5,6], Set.fromList [7,8], Set.fromList [9,10,11,12]])
        ]


-- |
reportGroupsByBatch :: [(Int,Int)] -> [Set Int]
reportGroupsByBatch pairs = mergeGroups . concatMap reportGroups $ batch10k pairs


-- |
mergeGroups :: Ord a => [Set a] -> [Set a]
mergeGroups groups = go [] groups
    where
    go :: Ord a => [Set a] -> [Set a] -> [Set a]
    go merged [] = merged
    go merged groups = uncurry go $ (flip List.insert merged . fst) &&& snd $ goInner (head groups) (tail groups)
        where
        goInner :: Ord a => Set a -> [Set a] -> (Set a, [Set a])
        goInner merge candidates = case List.partition (Set.null . Set.intersection merge) candidates of
            (emptyIntersect, []) -> (merge, emptyIntersect)
            (emptyIntersect, mergeThese) -> goInner (List.foldl' Set.union merge mergeThese) emptyIntersect


testMergeGroups :: [Bool]
testMergeGroups = runTests (List.sort . mergeGroups) tests
    where 
    tests = [
        ([ Set.fromList [2,3,4], Set.fromList [3,5,6], Set.fromList [6,8] ], [ Set.fromList [2,3,4,5,6,8] ]),
        ([ Set.fromList [2,3,4], Set.fromList [3,5,6], Set.fromList [8,9] ], [ Set.fromList [2,3,4,5,6], Set.fromList [8,9] ])
        ]


-- | transport groups of inmates at lowest bus cost to a new location
--
-- bus cost
--  they make it necessary to find all groups with their size
--  one group per bus
--
-- groups
--  have to determine the number of groups and their sizes
--  for example one group of size 4:
--      3-1-2-4 == [(3,1),(1,2),(2,4)]
--
-- 2-1-3, 4-5-6, 7-8
-- > transportInmatesCost 8 [(2,1),(1,3),(4,5),(5,6),(7,8)] == 6
-- 18-19-20-21-22-23
-- > transportInmatesCost 6 [(18,19),(19,20),(20,21),(21,22),(22,23)] == 3
-- 2-1-3, 4-5-6, 7-8, 9-10-11-12
-- > transportInmatesCost 12 [(2,1),(1,3),(4,5),(5,6),(7,8),(9,10),(10,11),(11,12)] == 8
--
-- 1-5-6-8, 7-3
-- > transportInmatesCost 8 [(8,1),(5,8),(7,3),(8,6)] == 6
-- 1-6-9-11-13-15-16, 12-14
-- > transportInmatesCost 16 [(6,11),(9,5),(11,9),(15,9),(13,15),(12,14),(15,16),(1,16)] == 11
transportInmatesCost :: Int -> [(Int,Int)] -> Int
transportInmatesCost n pairs = sum $ map busCost (replicate singleInmates 1 ++ groups)
    where
    groups = List.map Set.size $ reportGroupsByBatch pairs
    chainedInmates = sum groups
    singleInmates = n - chainedInmates
    busCost = ceiling . sqrt . fromIntegral


-- | convert a list to a list of pairs
-- > slicePairs [2,3,4,5] == [(2,3),(4,5)]
slicePairs :: [a] -> [(a, a)]
slicePairs [] = []
slicePairs (x:y:xs) = (x,y) : slicePairs xs
slicePairs [_singleValueLeft] = []


-- | parse the input as one string
-- reading each line separately is simpler
-- done this way to practice function composition
input :: IO (Int, [(Int, Int)])
input = 
    getContents <&>
        (head &&& (slicePairs . drop 2)) . map read . words


main :: IO ()
main = do
    -- print runAllTests
    input >>= print . uncurry transportInmatesCost
