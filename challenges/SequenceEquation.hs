module Main where

import Data.Map as Map
import Data.Maybe as Maybe
import Data.List as List
import System.Environment.Blank
import System.IO


-- | generate the list of indexes for a list
-- > genIx 1 [5,3,2] == [1,2,3]
genIx :: Int -> [Int] -> [Int]
genIx _ [] = []
genIx n (x:xs) = n:(genIx (n+1) xs)

-- | find the y such that p(p(y)) == x
--
-- example [5,2,1,3,4]
-- p(4) == 3, p(3) == 1
--   => p(p(4)) == 1
--
-- > sequenceEquation [5,2,1,3,4] == [4,2,5,1,3]
sequenceEquation :: [Int] -> Maybe [Int]
sequenceEquation pX = sequence $ List.map (\x -> return x >>= lookup >>= lookup) $ genIx 1 pX
  where
  inversePx = Map.fromList $ zip pX [1..]
  lookup x = Map.lookup x inversePx


-- | parse string from stdin
input :: IO [Int]
input = do
    getContents >>= return . parse . List.map read . words
    where
    parse numbers = (tail numbers)

main :: IO ()
main = do
    outFilePath <- getEnvDefault "OUTPUT_PATH" "/dev/stdout"
    withFile outFilePath WriteMode (\fptr -> do
        input >>=
            mapM_ ((hPutStrLn fptr) . show) . Maybe.fromMaybe [] . sequenceEquation
        )
