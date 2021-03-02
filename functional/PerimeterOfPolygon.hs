module Main where

type Point = (Int, Int)

euclid_distance :: Point -> Point -> Double
euclid_distance (p1, p2) (q1, q2) = sqrt . fromIntegral $ (p1 - q1)^2 + (p2 - q2)^2

solve :: [Point] -> Double
solve points =
  sum [euclid_distance x y | (x,y) <- zip points (tail points ++ [head points])]


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
