module Main where

fib :: Int -> Int
fib 1 = 0
fib 2 = 1
fib n = fst . foldl (\(p, q) _ -> (p+q, p)) (1, 0) $ [3..n]

main = do
  getLine >>= print . fib . (read :: String -> Int)
