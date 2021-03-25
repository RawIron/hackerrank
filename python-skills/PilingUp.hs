module Main (
  main
) where

pileUp :: [Int] -> Bool
pileUp plates = go emptyStack plates
  where
  emptyStack = (2^31,2^31)
  go stack plates
    | noMorePlates = True
    | cannotStack = False
    | takeLeft = go putLeftOnStack removeLeft
    | takeRight = go putRightOnStack removeRight
    where
    noMorePlates = null plates
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
  printAnswer $ pileUp [7,4,6]
