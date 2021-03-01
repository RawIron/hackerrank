module SumOfOdd where


f :: [Int] -> Int
f arr = sum . filter (not . even) $ arr

main = do
  inputdata <- getContents
  putStrLn $ show $ f $ map (read :: String -> Int) $ lines inputdata
