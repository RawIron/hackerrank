module Main where

import Control.Applicative
import System.IO

main :: IO ()
main = do
  n <- readOne :: IO Int
  let equations = createEquations n
  mapM_ printEquation equations

createEquations :: Int -> [(Int,Int,Int)]
createEquations n = map (\x -> (n, x, n*x)) [1..10]

printEquation :: (Int,Int,Int) -> IO ()
printEquation (n, x, nx) = do
  putStrLn $ show n ++ " x " ++ show x ++ " = " ++ show nx

readOne :: (Read a) => IO a
readOne = do
  read <$> getLine
