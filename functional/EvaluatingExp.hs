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

main :: IO()
main = do
    n <- readLn :: IO Int
    forM_ [1..n] $ \n_itr -> do
        x <- readLn :: IO Double
        printf "%.4f\n" . myexp $ x
