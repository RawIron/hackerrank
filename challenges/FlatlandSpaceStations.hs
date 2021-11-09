module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Data.List as List
import System.Environment.Blank
import System.IO


-- | calculate the longest distance from a city to nearest space station
-- | in the passed in interval
furthestFromStation :: (Int, Int) -> Int
furthestFromStation (a,b)
  | leftIsCity = b
  | rightIsCity = -b-1 - a
  | sameLocation = 0
  | nextToEachOther = 0
  | otherwise = quot (b - a) 2
  where
  leftIsCity = a == -1
  rightIsCity = b < 0
  sameLocation = a == b
  nextToEachOther = b - a == 1

-- | the longest distance of a city to its nearest space station
--
-- cities are on [0..totalCities]
-- space station locations are passed in
--   example longest distance is 2:
--     0 1 2 3 4
--       X X
flatlandSpaceStations :: Int -> [Int] -> Int
flatlandSpaceStations totalCities stations =
  maximum . map furthestFromStation $ zip (-1:stations) (stations ++ [-totalCities])


-- | tests
tests :: TestTree
tests = testGroup "Tests" [testFurthestFromStation, testFlatlandSpaceStations]

testFurthestFromStation :: TestTree
testFurthestFromStation = testGroup "FurthestFromStation"
  [ testCase "1 Station, left is City" $ furthestFromStation (-1,4) @?= 4,
    testCase "1 Station, right is City" $ furthestFromStation (1,-5) @?= 3
  ]

testFlatlandSpaceStations :: TestTree
testFlatlandSpaceStations = testGroup "FlatlandSpaceStation"
  [ testCase "1 City, 1 Station" $  flatlandSpaceStations 1 [0] @?= 0,
    testCase "2 Cities, 1 Station" $  flatlandSpaceStations 2 [1] @?= 1,
    testCase "3 Cities, 1 Station at last" $  flatlandSpaceStations 3 [2] @?= 2,
    testCase "3 Cities, 1 Station at first" $  flatlandSpaceStations 3 [0] @?= 2
  ]

runTests :: IO ()
runTests = do
    defaultMain tests


-- | parse string from stdin
input :: IO (Int, [Int])
input = do
    getContents >>= return . parse . map read . words
    where
    parse numbers = (head numbers, List.sort . tail . tail $ numbers)

solve :: IO ()
solve = do
    outFilePath <- getEnvDefault "OUTPUT_PATH" "/dev/stdout"
    withFile outFilePath WriteMode (\fptr -> do
        input >>=
            hPutStrLn fptr . show . uncurry flatlandSpaceStations
        )

main :: IO ()
main = do
    -- runTests
    solve
