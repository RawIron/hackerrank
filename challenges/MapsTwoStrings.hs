module Main where

import Control.Monad
import qualified Data.Set as Set
import System.Environment.Blank
import System.IO

-- | do the strings share a substring with length >= 1
-- > shareSubstr "yours" "sea" == True
shareSubstr :: String -> String -> Bool
shareSubstr str1 str2 =
    (not . Set.null) $ Set.intersection (Set.fromList str1) (Set.fromList str2)

-- | hackerrank output
showYesNo :: Bool -> String
showYesNo isTrue
    | isTrue = "YES"
    | otherwise = "NO"

-- | parse input string
input :: IO (String, String)
input = do
    r <- getLine
    s <- getLine
    return (r, s)

main :: IO()
main = do
    outFilePath <- getEnvDefault "OUTPUT_PATH" "/dev/stdout"
    withFile outFilePath WriteMode (\fptr -> do  
        t <- readLn :: IO Int
        forM_ [1..t] $ \t_itr -> do
            input >>= hPutStrLn fptr . showYesNo . uncurry shareSubstr
        )
