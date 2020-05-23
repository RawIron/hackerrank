module Main where

main :: IO()
main = do
    greetings <- getLine :: IO String
    putStrLn "Hello, World."
    putStrLn greetings
