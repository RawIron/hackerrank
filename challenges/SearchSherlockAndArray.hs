module Main where

import Control.Monad (forM_)
import System.Environment.Blank
import System.IO

-- | does an element exist such that
-- | sum [1..k-1] == sum [k+1..n]
--  
-- place the balance point at the far left
-- move the weight of the balance point to the left
-- move the weight from the right into the balance point
--
-- Example: 3 4 9 5 2
--
--   3                  3
-- 0   4 9 5 2        0   20
--
--     4                  4
-- 0 3   9 5 2        0+3   20-4
--
--       9                9
-- 0 3 4   5 2        3+4   16-9
--
-- > hasBalancePoint [4] == True
-- > hasBalancePoint [2,2] == False
-- > hasBalancePoint [3,4,9,5,2] == False
hasBalancePoint :: [Int] -> Bool
hasBalancePoint numbers@(_:ns)
  | null ns = True
  | otherwise = hasBalancePoint' (head numbers, 0, (sum . tail) numbers) (tail numbers)

hasBalancePoint' :: (Int, Int, Int) -> [Int] -> Bool
hasBalancePoint' (point, sumLeft, sumRight) (nextPoint:points)
  | sumLeft == sumRight = True
  | null points = False
  | otherwise = hasBalancePoint' (nextPoint, sumLeft + point, sumRight - nextPoint) points

-- | hackerrank output
showYesNo :: Bool -> String
showYesNo isTrue
  | isTrue = "YES"
  | otherwise = "NO"

-- | parse input string from stdin
input :: IO [Int]
input = do
  _ <- readLn :: IO Int
  getLine >>= return . map read . words

main :: IO ()
main = do
  outFilePath <- getEnvDefault "OUTPUT_PATH" "/dev/stdout"
  withFile outFilePath WriteMode (\fptr -> do  
    t <- readLn :: IO Int
    forM_ [1..t] $ \t_itr -> do
      input >>= hPutStrLn fptr . showYesNo . hasBalancePoint
    )
