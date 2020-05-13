{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.List.Split
import Data.Set
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe


readInput :: IO [String]
readInput = do
    _ <- readLn :: IO Int
    line <- getLine :: IO String
    return $ words $ line


main :: IO ()
main = do
    words <- readInput
    putStrLn $ unwords $ reverse $ words
