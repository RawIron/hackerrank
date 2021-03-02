module Main where

import Control.Monad
import Text.Printf


fact :: Int -> Int
fact n = product [2 .. n] 

exproximate :: Int -> Double -> Double
exproximate 0 _ = 1
exproximate n x = (x^n / (fromIntegral . fact) n) + exproximate (n-1) x

myexp :: Double -> Double
myexp x = exproximate 9 x


input :: IO [Double]
input = do
  n <- readLn :: IO Int
  inputSeq' n

-- use sequence
inputSeq' :: Int -> IO [Double]
inputSeq' n = sequence $ replicate n (readLn :: IO Double)

-- an implementation of sequence using recursion
-- the implementation in base is
--  sequence = mapM id
input' :: Int -> IO [Double]
input' 0 = return []
input' n = do
  x <- readLn :: IO Double
  xs <- input' (n-1)
  return (x: xs)


main :: IO ()
main = do
  numbers <- input
  mapM_ (printf "%.4f\n" . myexp) numbers
