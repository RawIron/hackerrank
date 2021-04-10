{-
  library for reading hackerrank-style inputs from stdin

  single value of simple type
    int, string, double
  read values for a, b, c
  values are on a single line and separated by space
    3 4 5
  one value per line
    3
    4
    5

  single value of compound type
  lists of aggregate types
    tuples, records, pairs
  read value for (a, b)
  values are separated by space
    3 4
 
  list of simple types
    int, string, double
  read values for []
  values are on a single line and separated by space
    3 4 5
  one value per line
    3
    4
    5

  list of compound types
    tuples, records, pairs
  read values for [(,)]
  one aggregated type per line and values are separated by space
    3 4
    5 6

  multi lists of simple type
  read values for [] []
  values are on a single line and separated by space
    3 4 5
    6 7 8
  one line with length of the list
  one value per line
  one line with length of the list
  one value per line
    3
    3
    4
    5
    3
    6
    7
    8
 -}

module HackerrankInOut where

import Control.Monad
import System.Environment
import System.IO

-- reads N values of the same type from a single input line
--
-- input format
-- 1 N
-- 2 a1 a2 .. aN
readOneLineMany :: (Read a) => IO [a]
readOneLineMany = do
  _ <- readLn :: IO Int
  getLine >>= return . map read . words

{-
 - input format
 - 1 N
 - 2 a1
 - 3 a2
 - ..
 - N aN
 -}
readManyLinesOne :: (Read a) => IO [a]
readManyLinesOne = do
  n <- readLn
  sequence $ replicate n readLn

-- reads many input lines
-- each line has N values of the same type
--
-- input format
-- 1 M N
-- 2 a1 a2 .. aN
-- 3 a1 a2 .. aN
-- ..
-- M a1 a2 .. aN
readManyLinesMany :: (Read a) => IO [[a]]
readManyLinesMany = do
  m <- (getLine >>= return . read . head . words)
  sequence $ replicate m (getLine >>= return . map read . words)
    

-- reads a single value
-- input format
-- 1 a1
readOne :: (Read a) => IO a
readOne = do
  readLn

{-
 - input format
 - 1 a1 a2
 -}
readOneLineTwo :: (Read a, Read b) => IO (a,b)
readOneLineTwo = do
  getLine >>= return . (\[x,y] -> (read x, read y)) . words


{-
 - input format
 - 1 a1
 - 2 a2
 -}
readTwoLinesOne :: (Read a, Read b) => IO (a,b)
readTwoLinesOne = do
  fstValue <- readLn
  sndValue <- readLn
  return (fstValue, sndValue)

{-
 - test read functions
 -}
testReadTwoLinesOne :: IO ()
testReadTwoLinesOne = do
  pair <- readTwoLinesOne :: IO (Int,Int)
  (putStrLn . show) $ fst pair
  (putStrLn . show) $ snd pair

testReadManyLinesOne :: IO ()
testReadManyLinesOne = do
  readManyLinesOne >>= mapM_ putStrLn


-- | 
write :: (Show a) => a -> IO ()
write value = do
    outputPath <- getEnv "OUTPUT_PATH"
    fptr <- openFile outputPath WriteMode
    hPutStr fptr $ show value
    hFlush fptr
    hClose fptr

{-
 - putting it all together
 -}

-- | solve a challenge
solveV1 :: a -> [String]
solveV1 _ = ["the", "solution"]

solveV4 :: a -> a
solveV4 x = x

solveV2 :: [a] -> [String]
solveV2 _ = ["the", "solution"]

solveV3 :: a -> [a] -> [String]
solveV3 _ _ = ["the", "solution"]


-- | consume all lines with one value per line
readNv1 :: (Read a) => IO [a]
readNv1 = do
  n <- readLn :: IO Int 
  getContents >>= return . map read . lines

-- | consume all values with each word is a value
readNv2 :: (Read a) => IO [a]
readNv2 = do
  n <- readLn :: IO Int 
  getContents >>= return . map read . words

read1v1 :: (Read a) => IO a
read1v1 = do
  readLn

-- | bring read and solve together
mainv1 :: IO ()
mainv1 = do
  (read1v1 :: IO Int) >>= putStrLn . show . solveV4

mainv2 :: IO ()
mainv2 = do
  (readNv1 :: IO [Int]) >>=
    mapM_ putStrLn . (\(x:xs) -> solveV3 x xs)

mainv3 :: IO ()
mainv3 = do
  t <- readLn :: IO Int
  forM_ [1..t] $ \t_itr -> do
    (read1v1 :: IO Double) >>= putStrLn . show . solveV4
