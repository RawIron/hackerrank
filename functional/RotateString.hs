module Main where

solve :: String -> [String]
solve word =
    tail . reverse $ foldl (\(w:ws) _ -> (rotate w) : w : ws) [word] [1..n]
    where
        rotate w = (tail w ++ [head w])
        n = length word

input :: IO [String]
input = getContents >>= return . tail . lines

main :: IO ()
main = do
    input >>= mapM_ (putStrLn . unwords) . map solve
