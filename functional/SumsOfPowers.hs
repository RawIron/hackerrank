module Main where

-- | n-th root of x
--
-- > nroot 2 4 == 2
-- > nroot 2 16 == 4
-- > nroot 3 8 == 2
-- > nroot 3 27 == 3
nroot :: (Integral a, Floating b) => a -> b -> b 
nroot n x = x ** (1 / fromIntegral n)


-- | In how many ways can a number be expressed as the sum of unique squares or cubes or ..
-- | Example: 10 == 9 + 1 = 3^2 + 1^2
--
-- Finding the solutions for 21 can be done with a search on this tree
--
--           16             9      4    1
--         9    4   1      4 1    1
--        4 1  1          1
--       1
--
-- > sumsOfPowers 100 2 == 3
-- > sumsOfPowers 29 2 == 2
-- > sumsOfPowers 67 2 == 0
sumsOfPowers :: Int -> Int -> Int
sumsOfPowers number power = uniquePowers number powers
  where
  upperBound = floor $ nroot power $ fromIntegral number
  powers = map (^power) [upperBound,upperBound-1..1] 

-- | traverse the search tree and count the solutions
-- > 10 [9,4,1] == 1
-- > 29 [25,16,9,4,1] == 2
--
-- Example 29 == 2
-- 29 5..1
-- 29 [5..1, 4..1, 3..1, 2..1, 1..1] $ filter (<=29) $ map (^2) 3..1
--   29-5^2=4 4..1
--     4 [2..1, 1..1] $ filter (<=4) $ map (^2) 4..1
--       4-2^2=0 1..1
--         [5,2]
--       4-1^2 []
--        []
--   29-4^2=13 3..1
--     13 [3..1, 2..1, 1..1] $ filter (<=13) $ map (^2) 3..1
--       13-3^2=4 2..1
--         4 [2..1, 1..1] $ filter (<=4) $ map (^2) 2..1
--           4-2^2=0 1..1
--             [4,3,2]
--           4-1^2 []
--             []
--       13-2^2=9 1..1
--         9 > sum $ map (^2) 1..1
--       13-1^2=12 []
--         []
--   29-3^2=20 2..1
--     20 > sum $ map (^2) 2..1
--   29-2^2=25 1..1
--     25 > sum $ map (^2) 1..1
--   29-1^2=28 []
--     []
uniquePowers :: Int -> [Int] -> Int
uniquePowers n powers
  | n == 0 = 1
  | null powers = 0
  | n > sum powers = 0
  | otherwise = foldl (\acc child -> (+) acc $ uniquePowers (n - head child) (tail child)) 0 $ children $ filter (<=n) powers

-- | generate the children of a node in our search tree
-- > children [9,4,1] == [[9,4,1],[4,1],[1]]
children :: [Int] -> [[Int]]
children powers = foldr go [] powers
  where
  go p acc
    | null acc = [p] : []
    | otherwise = (p:head acc):acc


-- | convert a list of size 2 into a pair
-- > parify [2,3] == (2,3)
pairify :: [a] -> (a,a)
pairify [x,y] = (x,y)

-- | parse input
input :: IO (Int, Int)
input = getContents >>= return . pairify . map read . words

main = do
  input >>= print . uncurry sumsOfPowers
