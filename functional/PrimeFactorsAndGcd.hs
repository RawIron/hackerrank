module Main where

testGcdFromPrimes = ([
  [(2,2),(3,2),(5,3)],
  [(3,2),(5,3),(11,1)],
  [(2,2),(3,3),(5,4),(7,6),(19,18)]
  ],
  [(3,2),(5,3)])

gcdFromPrimes :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
gcdFromPrimes ps qs = go [] ps qs where
    go primes _ [] = reverse primes
    go primes [] _ = reverse primes
    go primes ((p,pExp):ps) ((q,qExp):qs)
        | p == q = go ((q, (min pExp qExp)):primes) ps qs
        | p > q = go primes ((p,pExp):ps) qs
        | p < q = go primes ps ((q,qExp):qs)

solve :: [[(Int, Int)]] -> [(Int, Int)]
solve primeFactorsOfNumbers =
    foldl gcdFromPrimes (head primeFactorsOfNumbers) (tail primeFactorsOfNumbers)

-- identical to sliceVertPair implementation
slicePairs :: [Int] -> [(Int, Int)]
slicePairs = go where
  go (x:y:xs) = (x,y) : go xs
  go (x:[]) = []
  go [] = []

input :: IO [[(Int, Int)]]
input = do
  n <- readLn :: IO Int
  getContents >>= return . map (slicePairs . map read . words) . lines

printPair :: (Int, Int) -> IO ()
printPair (p, q) = putStr $ (show p) ++ " " ++ (show q) ++ " "

main :: IO ()
main = do
  input >>= mapM_ printPair . solve
