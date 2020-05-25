module Main where

import System.IO

main :: IO ()
main = do
    n <- readOne :: IO Int
    let numbers = readFixedLines n :: [IO Int]
    mapM_ (>>= (display . isPrime)) numbers

display :: Bool -> IO ()
display True = putStrLn "Prime"
display False = putStrLn "Not prime"

isPrime :: Int -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime number
    | isEven number = False
    | isDivisible number = False
    | otherwise = True
    where
      sqrtCeil = (floor . sqrt . fromIntegral)
      isDivisible number = hasDivisor number 3 (sqrtCeil $ number)

hasDivisor :: Int -> Int -> Int -> Bool
hasDivisor number lower upper
    | lower > upper = False
    | number `mod` lower == 0 = True
    | otherwise = hasDivisor number (lower+2) upper

isEven :: Int -> Bool
isEven number
    | number `mod` 2 == 0 = True
    | otherwise = False

readOne :: (Read a) => IO a
readOne = do
    read <$> getLine

readFixedLines :: (Read a) => Int -> [IO a]
readFixedLines n
    | n <= 0 = []
    | otherwise = do          
        (read <$> getLine) : readFixedLines (n-1)
