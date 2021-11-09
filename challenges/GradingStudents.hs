module Main where

import Data.List as List
import System.Environment.Blank
import System.IO


-- | adjust pass grades which are
-- | 1 or 2 points lower than the next multiple of 5
--
-- > adjustStudentGrade 49 == 50
-- > adjustStudentGrade 41 == 41
-- > adjustStudentGrade 65 == 65
adjustStudentGrade :: Int -> Int
adjustStudentGrade grade
  | grade < 38 = grade
  | mod grade 5 == 0 = grade
  | adjustedGrade - grade < 3 = adjustedGrade
  | otherwise = grade
  where
  adjustedGrade = (quot grade 5) * 5 + 5


-- | parse string from stdin
input :: IO [Int]
input = do
    getContents >>= return . parse . map read . words
    where
    parse numbers = (tail numbers)

main :: IO ()
main = do
    outFilePath <- getEnvDefault "OUTPUT_PATH" "/dev/stdout"
    withFile outFilePath WriteMode (\fptr -> do
        input >>=
            mapM_ ((hPutStrLn fptr) . show . adjustStudentGrade)
        )
