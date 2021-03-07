module Main where

solve :: [(Char,Char)] -> String
solve = foldr (\(x,y) acc -> y:x:acc) []

slicePairs :: [a] -> [(a, a)]
slicePairs = go where
  go (x:y:xs) = (x,y) : go xs
  go [] = []
  go _ = []

input :: IO [String]
input = do
  n <- readLn :: IO Int
  getContents >>= return . words

main :: IO ()
main = do
  input >>= mapM_ (putStrLn . solve . slicePairs)
