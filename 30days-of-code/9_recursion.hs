{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import qualified Data.List as L
import Data.List.Split
import Data.Set
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe


factorialFold :: Int -> Int
factorialFold n = L.foldl (\x acc -> x * acc) 1 [1..n]

factorialRecursive :: Int -> Int
factorialRecursive n = factorial' n 1
    where
    factorial' 0 acc = acc
    factorial' m acc = factorial' (m-1) (m * acc)

factorial :: Int -> Int
factorial = factorialRecursive


write :: Int -> IO ()
write number = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    hPutStrLn fptr $ show number

    hFlush fptr
    hClose fptr


main :: IO()
main = do
    n <- readLn :: IO Int
    let result = factorial n
    write result
