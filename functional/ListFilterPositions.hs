module ListFilterPositions where


f :: [Int] -> [Int]
f lst = map (snd) . filter ((0 ==) . flip mod 2 . fst) . zip [1..length lst] $ lst

main :: IO ()
main = do
	inputdata <- getContents
	mapM_ (putStrLn . show) . f . map read . lines $ inputdata
