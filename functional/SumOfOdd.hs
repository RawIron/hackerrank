module SumOfOdd where

-- | sum odd numbers in an array
-- > f [1,2,4,5] == 6
f :: [Int] -> Int
f arr = sum . filter (not . even) $ arr

main :: IO ()
main = do
  getContents >>= putStrLn . show . f . map read . lines
