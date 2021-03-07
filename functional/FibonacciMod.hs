module Main where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fst . foldl (\(p, q) _ -> (p+q, p)) (1, 0) $ [2..n]

solve :: Integer -> Integer
solve n = fib n `mod` (10^8 + 7)

main :: IO ()
main = do
    getContents >>=
        mapM_ (putStrLn . show . solve . read) . tail . lines
