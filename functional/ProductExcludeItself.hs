module Main where

prodDiffZeros :: [Int] -> (Int, [Int])
prodDiffZeros numbers = (areAllZero numbers) $ calcProdAndZeros numbers
  where
  areAllZero numbers result@(_, zeros)
    | length numbers == length zeros = (0, zeros)
    | otherwise = result
  calcProdAndZeros numbers = foldl go (1, []) $ zip numbers [1..]
    where
    go (product, zeros) (number, index)
      | number == 0 = (product, index : zeros)
      | otherwise = (number * product, zeros)

prodWithoutItself :: [Int] -> [Int]
prodWithoutItself numbers = (calcProduct numbers) $ prodDiffZeros numbers
  where
  calcProduct numbers (prod, zeros)
    | manyZeros = replicate n 0
    | oneZero = (replicate (zeroIndex-1) 0) ++ [prod] ++ (replicate (n-zeroIndex) 0)
    | noZero = map (div prod) numbers
    where
    noZero = zeroCount == 0
    oneZero = zeroCount == 1
    manyZeros = zeroCount > 1
    zeroCount = length zeros
    zeroIndex = head zeros
    n = length numbers

test_prodWithoutItself :: [[Int]]
test_prodWithoutItself = foldl go [] tests
  where
  tests = [([1,2,3], [6,3,2]),
           ([0,0,0], [0,0,0]),
           ([0,2,0], [0,0,0]),
           ([5,2,0], [0,0,10]),
           ([0,4,3], [12,0,0])
          ]
  go failedTests test
    | resultEqualsExpected = failedTests
    | otherwise = (fst test) : failedTests
    where
    resultEqualsExpected = ((prodWithoutItself $ fst test) == snd test)

main :: IO ()
main = do
  mapM_ (putStrLn . show) $ test_prodWithoutItself
