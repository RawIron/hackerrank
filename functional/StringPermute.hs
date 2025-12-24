module Main where

-- | swap characters in each pair
-- | concat characters into a string
-- >  solve [(a,b), (c,d)] == "badc"
solve :: [(Char,Char)] -> String
solve = foldr (\(x,y) acc -> y:x:acc) []

-- | convert a list to a list of pairs
-- >  slicePairs [a,b,c,d] == [(a,b), (c,d)]
slicePairs :: [a] -> [(a, a)]
slicePairs = go
  where
  go (x:y:xs) = (x,y) : go xs
  go [] = []
  go _ = []


input :: IO [String]
input = do
  _ <- readLn :: IO Int
  getContents >>= return . words

main :: IO ()
main = do
  input >>= mapM_ (putStrLn . solve . slicePairs)
