module Main where

import Data.List as L

-- | Calculate the expected win from playing Stein's Game with N cards
-- > steinGame 3 == 1.8333
steinGame :: Integer -> Float
steinGame nCards = (fromIntegral totalWins) / (fromIntegral totalCardPermutations)
  where
  cardPermutations = L.permutations [1..nCards]
  totalWins = sum $ map countHighs cardPermutations
  totalCardPermutations = factorial nCards

-- | count how many times a new highest value was found
-- > countHighs [2,1,5,3,4] == 2
countHighs :: [Integer] -> Integer
countHighs numbers = snd $ foldl go (head numbers, 1) $ tail numbers
  where
  go (high, count) number
    | number > high = (number, count+1)
    | otherwise = (high, count)

-- | factorial of N
-- > factorial 3 == 6
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = foldl (*) 1 [1..n]

main :: IO ()
main = getContents >>= putStrLn . show . steinGame . read
