module Main where

pairify :: [a] -> (a,a)
pairify [x,y] = (x,y)

mingle :: String -> String -> String
mingle [] [] = []
mingle fstWord sndWord =
    (head fstWord) : (head sndWord) : mingle (tail fstWord) (tail sndWord)

main :: IO ()
main = do
  getContents >>= putStrLn . uncurry mingle . pairify . words
