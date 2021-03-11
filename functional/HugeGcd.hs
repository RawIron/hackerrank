{-
   GCD - Euclid's Algorithm

  4598 = 3211 * 1 + 1387
  3211 = 1387 * 2 + 437
  1387 = 437 * 3 + 76
  437 = 76 * 5 + 57
  76= 57 * 1 + 19
  57 = 19 * 3 + 0

  4598
   = 3211 * 1
     + 1387

   = (1387 * 2 + 437) * 1
     + 1387

   = ((437 * 3 + 76) * 2
        + 437
     )
     + (437 * 3 + 76)

   = (((76 * 5 + 57) * 3 + 76) * 2
        + (76 * 5 + 57)
     )
     + ((76 * 5 + 57) * 3 + 76)

   = ((((57 * 1 + 19) * 5 + 57) * 3 + (57 * 1 + 19)) * 2
        + ((57 * 1 + 19) * 5 + 57)
     )
     + (((57 * 1 + 19) * 5 + 57) * 3 + (57 * 1 + 19))

   = (((((19 * 3) * 1 + 19) * 5 + (19 * 3)) * 3 + ((19 * 3) * 1 + 19)) * 2
        + (((19 * 3) * 1 + 19) * 5 + (19 * 3))
     )
     + ((((19 * 3) * 1 + 19) * 5 + (19 * 3)) * 3 + ((19 * 3) * 1 + 19))
 -}


module Main where

import Data.List
import Control.Monad (join)
import Control.Arrow ((***))

both :: (a -> b) -> (a, a) -> (b, b)
both = join (***)

-- Converts a curried function to a function on a triple.
uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f ~(a,b,c) = f a b c


padShorter :: [Integer] -> [Integer] -> ([Integer], [Integer])
padShorter x y
    | length x > length y = (x, y ++ repeat 1)
    | otherwise = (x ++ repeat 1, y)

factorGcd :: [Integer] -> [Integer] -> (Integer, [Integer], [Integer])
factorGcd xl yl =
    foldl gcdOnPairs (1, [], []) $ zip xl yl
    where
        gcdOnPairs (gcdFactor, xs, ys) (x, y) =
            (gcdPair * gcdFactor, (div x gcdPair) : xs, (div y gcdPair) : ys)
            where
                gcdPair = gcd x y

gcdWithFactor :: Integer -> [Integer] -> [Integer] -> Integer
gcdWithFactor factors xs ys = factors * gcd (product xs) (product ys)

useShorten :: [Integer] -> [Integer] -> Integer              
useShorten xs ys =
    uncurry3 gcdWithFactor . uncurry factorGcd . uncurry padShorter . both sort $ (xs, ys)


usePrelude :: [Integer] -> [Integer] -> Integer
usePrelude xs ys = gcd (product xs) (product ys)

solve :: [Integer] -> [Integer] -> Integer
solve xs ys = (useShorten xs ys) `mod` (10^9 + 7)


input :: IO [[Integer]]
input = do
    getContents >>=
        return . map strToIntegers . keepOnlyList . lines
    where
        keepOnlyList = map snd . filter fst . (zip [False, True, False, True])
        strToIntegers = map (read :: String -> Integer) . words

main :: IO ()
main = do
    (listA : listB : _) <- input
    putStrLn . show $ solve listA listB
