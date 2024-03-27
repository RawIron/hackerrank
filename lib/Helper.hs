module Helper where

-- | collect info about runtime environment
import Control.Concurrent (rtsSupportsBoundThreads)
import GHC.Conc (numCapabilities)

data Runtime = Runtime {
    isThreaded :: Bool,
    cores :: Int
} deriving (Show)

reportRuntime :: Runtime
reportRuntime = Runtime rtsSupportsBoundThreads numCapabilities


-- | convert a list to a list of pairs
-- > slicePairs [2,3,4,5] == [(2,3),(4,5)]
slicePairs :: [a] -> [(a, a)]
slicePairs [] = []
slicePairs [x] = []
slicePairs (x:y:xs) = (x,y) : slicePairs xs

-- | convert a list of pairs to a list
-- > concatPairs [(2,3),(4,5)] == [2,3,4,5]
concatPairs :: [(a,a)] -> [a]
concatPairs [] = []
concatPairs ((x,y):xs) = x:y : concatPairs xs

-- | apply two functions on the same value
-- identical to @f &&& g@
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
-- identical to @drop 2@
-- > tail2 [1,2,3,4] == [3,4]
tail2 :: [a] -> [a]
tail2 = tail . tail
