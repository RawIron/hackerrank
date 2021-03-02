module Main where

type Point = (Int, Int)

euclid_distance :: Point -> Point -> Double
euclid_distance (p1, p2) (q1, q2) = sqrt . fromIntegral $ (p1 - q1)^2 + (p2 - q2)^2

solve :: [Point] -> Double
solve points = sum [euclid_distance x y |  (x,y) <- zip points (tail points ++ [head points])]


parsePairs :: [Int] -> [(Int, Int)]
parsePairs [] = []
parsePairs (x:y:xs) = (x,y) : parsePairs xs

main :: IO ()
main = do
  n <- readLn :: IO Int
  input <- getContents
  putStrLn . show . solve . parsePairs $ (map read (words input) :: [Int])
