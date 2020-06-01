{-# LANGUAGE FlexibleInstances #-}

module Main where

import Data.String


main :: IO ()
main = do
  inWords <- readWords
  let (fstWords, int_array) = readMany inWords :: ([String], [Int])
  let (sndWords, str_array) = readMany fstWords :: ([String], [String])

  printArray int_array
  printArray str_array


-- with show strings are printed with surrounding quotes
-- show <string> == "<string>"
printArray :: (Conv a) => [a] -> IO ()
printArray anArray = mapM_ (putStrLn . toStr) anArray


readWords :: IO [String]
readWords = do
  words <$> getContents

-- read :: String -> String
-- throws an exception
readMany :: (Conv a) => [String] -> ([String], [a])
readMany words = (remainWords, values)
  where
    n = read $ head $ words :: Int
    values = (map fromStr) $ take n $ tail $ words
    remainWords = drop (n+1) words


class (Read a, Show a) => Conv a where
  toStr :: a -> String
  toStr = show
  fromStr :: String -> a
  fromStr = read

-- use default implementation
instance Conv Int

instance Conv [Char] where
  toStr = id
  fromStr = id
