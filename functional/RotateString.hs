module Main where

rotate :: String -> String
rotate w = (tail w ++ [head w])

recursive :: String -> [String]
recursive word =
    reverse $ go (length word) [rotate word]
    where
        go k (w : ws)
            | k == 1 = w : ws
            | otherwise = go (k-1) (rotate w : w : ws)

-- | rotate a word character by character
-- for one full rotation
--
-- > folding "fold" == ["oldf", "ldfo", "dfol", "fold"]
folding :: String -> [String]
folding word =
    tail . reverse $ foldl (\(w:ws) _ -> (rotate w) : w : ws) [word] [1..n]
    where  
        n = length word

solve :: String -> [String]
solve = recursive

input :: IO [String]
input = getContents >>= return . tail . lines

main :: IO ()
main = do
    input >>= mapM_ (putStrLn . unwords) . map solve
