module ListReplication where

replicateEach :: Int -> [Int] -> [Int]
replicateEach n numbers = concat $ map (replicate n) numbers

main :: IO ()
main = do
  getContents >>=
    mapM_ print . (\(x:xs) -> replicateEach x xs) . map read . words
