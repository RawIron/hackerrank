module Main (
  main
) where

pileUp :: [Int] -> Bool
pileUp plates = go (2^31,2^31) plates
  where
  go _ [] = True
  go stack plates
    | cannotStack = False
    | takeLeft = go putLeftOnStack removeLeft
    | takeRight = go putRightOnStack removeRight
    where
    cannotStack = fst stack < snd stack
    takeLeft = (head plates) >= (last plates)
    putLeftOnStack = (snd stack, head plates)
    removeLeft = (tail plates)
    takeRight = (head plates) < (last plates)  
    putRightOnStack = (snd stack, last plates)
    removeRight = (init plates)

printAnswer :: Bool -> IO ()
printAnswer canBeSolved
  | canBeSolved = putStrLn "Yes"
  | otherwise = putStrLn "No"

main :: IO ()
main = do
  printAnswer $ pileUp [7,5,9,6]
