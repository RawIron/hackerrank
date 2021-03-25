module Main (
  main
) where

{- | can smaller bumper plates always be stacked on larger bumper plates
 - | until there are no plates left on the bar?
 -  a bar has several bumper plates of different weights/diameter
 -      -4-6-8-3-1-
 -  in the above example the plate "4" or the plate "1" can be removed from the bar
 -
 -  on every move take the larger of the two accessable bumper plates from the bar
 -  and put it on top of the stacked plates
 -
 - > pileUp [4,6,8,3,1] == False
 - > pileUp [5,4,1]] == True
 -}
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


-- | print a bool value as Yes/No
-- this is not a good solution
-- use composition instead
printAnswer :: Bool -> IO ()
printAnswer canBeSolved
  | canBeSolved = putStrLn "Yes"
  | otherwise = putStrLn "No"

-- | print a bool value as Yes/No
-- (putStrLn . show . bool2YesNo)
data YesNo = Yes | No deriving (Show)

bool2YesNo :: Bool -> YesNo
bool2YesNo isTrue
  | isTrue = Yes
  | otherwise = No

main :: IO ()
main = do
  (putStrLn . show . bool2YesNo . pileUp) $ [7,4,6]
