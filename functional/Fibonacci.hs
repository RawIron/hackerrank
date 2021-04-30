module Main where

-- | the n-th Fibonacci Number
-- > fib 4 == 3
-- > fib 11 == 89
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fst . foldl (\(p, q) _ -> (p+q, p)) (1, 0) $ [2..n]

main = do
  getLine >>= print . fib . (read :: String -> Int)
