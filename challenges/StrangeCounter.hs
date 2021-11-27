module Main where

-- | read value of the counter at time t
--
-- the counter works as follows:
--   0st countdown from 3
--   1st countdown from 3 * 2
--   2nd countdown from (3 * 2) * 2
--
--    0               1                  2              3
-- 1  2  3    4  5  6  7  8  9   10 11 12 .. 21   22 23 ..
-- 3  2  1    6  5  4  3  2  1   12 11 10 ..  1   24 23 ..
--
-- a new countdown starts at time
--   3 * (2^n -1) + 1
--
-- in which countdown does time t fall?
--   t <= 3 * sum (2^n)
--     sum (2^n) == (2^(n+1)) - 1
--   solve for n
--   (log2 (t+3/3)) - 1 <= n
--
-- > readCounterAt 7 == 3
-- > readCounterAt 15 == 7
-- > readCounterAt 22 == 24
readCounterAt :: Int -> Int
readCounterAt t = countdownFrom - (t - timeAtCountdownStart)
  where
  float_t = fromIntegral t
  countdown = ceiling $ (logBase 2 ((float_t + 3)/3)) - 1
  countdownFrom = 3 * 2 ^ countdown 
  timeAtCountdownStart = ((3 * (2 ^ countdown - 1)) + 1)

-- | parse string from stdin
input :: IO Int
input = do
  getContents >>= return . read

main :: IO ()
main = do
  input >>= putStrLn . show . readCounterAt
