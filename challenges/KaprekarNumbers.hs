module Main where

import qualified Data.Text as T

-- | split a number after n digits counted
-- | from the right
-- > split 4 123456 == (12,3456)
-- > split 3 123 == (0,123)
split :: Int -> Int -> (Int, Int)
split d number
  | digitsInNumber == d = (0, number)
  | otherwise = (remaining, countedDigits)
  where
  numberText = T.pack . show $ number
  digitsInNumber = T.length numberText
  take n t = (read . T.unpack . T.take n) $ t
  drop n t = (read . T.unpack . T.drop n) $ t
  remaining = take (digitsInNumber - d) numberText
  countedDigits = drop (digitsInNumber - d) numberText

-- | is the number a Kaprekar Number
-- > isKaprekar 99 == True
isKaprekar :: Int -> Bool
isKaprekar number = (number == sumOfSplit)
  where
  digitsInNumber = length . show $ number
  sumOfSplit = (uncurry (+)) $ split digitsInNumber (number^2)

-- | return Kaprekar Numbers in a given range
-- > kaprekarNumbers 1 10 == [1,9]
kaprekarNumbers :: Int -> Int -> [Int]
kaprekarNumbers from to = filter isKaprekar [from .. to]

-- | place the first and second elem of a list into a tuple
pairify :: [Int] -> (Int, Int)
pairify (x:y:_) = (x, y)

-- | format output
putResult :: [Int] -> IO ()
putResult rs
  | null rs = putStrLn "INVALID RANGE"
  | otherwise = mapM_ (putStr . (++ " ") . show) rs

-- | parse input from stdin
input :: IO (Int, Int)
input = do
    getContents >>= return . pairify . map read . words

main :: IO()
main = do
    input >>= putResult . uncurry kaprekarNumbers
