module Main where

import System.Environment.Blank
import System.IO


uncurry3 :: ([a] -> b -> c -> d) -> (([a], b, c) -> d)
uncurry3 f (x, y, z) = f x y z

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x


-- | count all the slices in the array
-- | where it's sum == sumTarget
--
--  does not calc the sum for every slice
--  instead
--      calc the sum for the first slice
--      do a rolling sum for all other slices
--
-- > birthday [5,1,4,6,2,3] 5 2 == 2
birthday :: [Int] -> Int -> Int -> Int
birthday numbers sumTarget sizeSlice = fstCount + counted
  where
  fstSlice = take sizeSlice numbers
  fstCount
    | sum fstSlice == sumTarget = 1
    | otherwise = 0

  counted = fst3 . foldl go (0, fstSlice, sum fstSlice) $ drop sizeSlice numbers
  go (count, slice, sumSlice) elem
    | sumR == sumTarget = (count+1, sliceR, sumR)
    | otherwise = (count, sliceR, sumR)
    where
    sliceR = tail slice ++ [elem]
    sumR = sumSlice - head slice + elem


output :: Int -> IO()
output result = do
  outFilePath <- getEnvDefault "OUTPUT_PATH" "/dev/stdout"
  withFile outFilePath WriteMode (\fptr -> do
          hPutStrLn fptr . show $ result
      )


input :: IO( ([Int], Int, Int) )
input = do
  _ <- readLn :: IO Int
  
  numbers <-
    getLine >>= return . map read . words
  
  (sliceSum, sliceLen) <-
    getLine >>= return . (\[x,y] -> (read x, read y)) . words

  return (numbers, sliceSum, sliceLen)


main :: IO()
main = do
  input >>= output . uncurry3 birthday
