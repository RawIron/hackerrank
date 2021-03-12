module Main where

-- | combine two words into one like a zipper
--
-- b_l_a_c_k
-- _f_o_r_e_s_t
-- bfloarceks
mingle :: String -> String -> String
mingle [] _ = []
mingle _ [] = []
mingle fstWord sndWord =
    (head fstWord) : (head sndWord) : mingle (tail fstWord) (tail sndWord)

pairify :: [a] -> (a,a)
pairify [x,y] = (x,y)

input :: IO (String, String)
input = getContents >>= return . pairify . words

main :: IO ()
main = do
  input >>= putStrLn . uncurry mingle
