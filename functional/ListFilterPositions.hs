module ListFilterPositions where


evenPositions :: [Int] -> [Int]
evenPositions lst = map snd . filter ((0 ==) . flip mod 2 . fst) . zip [1..length lst] $ lst

main :: IO ()
main = do
  getContents >>= mapM_ (putStrLn . show) . evenPositions . map read . lines
