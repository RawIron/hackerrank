module Main where

type Point = (Int, Int)

combine :: Point -> Point -> Int
combine (p1, p2) (q1, q2) = (p1 * q2) - (p2 * q1)

-- | change path encoding from (p1-p2-p3-..-pn) to (p1-p2,p2-p3,p3-p4,..,pn-1-pn)
pathAsPairs :: [(Int, Int)] -> [((Int, Int), (Int, Int))]
pathAsPairs points = zip points (tail points ++ [head points])

solve :: [Point] -> Double
solve points =
  (abs . (/2) . fromIntegral . sum) [combine x y | (x,y) <- pathAsPairs points]


-- identical to sliceVertPair implementation
slicePairs :: [Int] -> [(Int, Int)]
slicePairs = go where
  go (x:y:xs) = (x,y) : go xs
  go [] = []
  go _ = []

input :: IO [(Int, Int)]
input = do
  n <- readLn :: IO Int
  getContents >>= return . slicePairs . map read . words


main :: IO ()
main = do
  input >>= putStrLn . show . solve
