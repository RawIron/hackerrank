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


calcTotal :: Double -> Int -> Int -> Double
calcTotal meal_cost tip_percent tax_percent = total
    where
        tip = meal_cost * (fromIntegral tip_percent/100.0)
        tax = meal_cost * (fromIntegral tax_percent/100.0)
        total = meal_cost + tip + tax

readInput :: IO (Double, Int, Int)
readInput = do
    meal <- readLn :: IO Double
    tip <- readLn :: IO Int
    tax <- readLn :: IO Int
    return (meal, tip, tax)


main :: IO ()
main = do
    (meal_cost, tip_percent, tax_percent) <- readInput
    
    print $ round $ calcTotal meal_cost tip_percent tax_percent
