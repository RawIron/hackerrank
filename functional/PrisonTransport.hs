module Main where

import Control.Arrow ((&&&))

solve :: Int -> [(Int, Int)] -> [Int] -> Int
solve n pairs unpairs = n

-- | convert a list to a list of pairs
-- identical to sliceVertPair implementation
-- > slicePairs [2,3,4,5] == [(2,3),(4,5)]
slicePairs :: [a] -> [(a, a)]
slicePairs = go where
  go (x:y:xs) = (x,y) : go xs
  go (x:[]) = []
  go [] = []

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
input :: IO (Int, [(Int, Int)], [Int])
input = 
    getContents >>=
        return . (forkTriple head (slicePairs . tail2) (id . tail2)) . map read . words

main :: IO ()
main = do
    input >>= putStrLn . show . uncurry3 solve
