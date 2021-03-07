module Main where

fact :: Int -> Int
fact n = foldl (*) 1 [1..n]

inomCoeff :: Int -> Int -> Int
binomCoeff n r
    | n == 0 || r == 0 = 1
    | n - r >= r = (foldl (*) 1 [(n-r+1)..n]) `div` (fact r)
    | otherwise = (foldl (*) 1 [r+1..n]) `div` (fact (n-r))
    
pascalTriangleRow :: Int -> [Int]
pascalTriangleRow k
    | even (k+1) = leftHalf ++ reverse leftHalf
    | otherwise = leftHalf ++ middleValue ++ reverse leftHalf
    where
        left = ((k+1) `div` 2) - 1
        leftHalf = map (binomCoeff k) [0..left]
        middleValue = [binomCoeff k (left+1)]

showRow :: [Int] -> String
showRow = (unwords . map show)

main :: IO ()
main = do
    n <- readLn :: IO Int
    (mapM_ putStrLn . map showRow . map pascalTriangleRow) $ [0..n-1]
