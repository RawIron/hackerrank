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

padShorter :: [Integer] -> [Integer] -> [[Integer]]
padShorter x y
    | length x > length y = [x, y ++ repeat 1]
    | otherwise = [x ++ repeat 1, y]

shorten :: [Integer] -> [Integer] -> (Integer, [Integer], [Integer])
shorten x y =
    foldl gcdOnPairs (1, [], []) $ zip sortedX sortedY
    where
        (sortedX : sortedY : _) = padShorter (sort x) (sort y)
        gcdOnPairs (gcdFactor, xs, ys) (a, b) =
            (gcdPair * gcdFactor, (div a gcdPair) : xs, (div b gcdPair) : ys)
            where
                gcdPair = gcd a b

useShorten :: [Integer] -> [Integer] -> Integer              
useShorten a b =
    gcdFactor * gcd (product smallA) (product smallB)
    where
        (gcdFactor, smallA, smallB) = shorten a b

usePrelude :: [Integer] -> [Integer] -> Integer
usePrelude a b = gcd (product a) (product b)

solve :: [Integer] -> [Integer] -> Integer
solve a b = (useShorten a b) `mod` (10^9 + 7)

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
