-- How to calculate the number of ways to sum to 100?
-- First calculate all the ways to sum to n w/ 1 as the max number, for n from 1 to 100
-- Then calculate all the ways to sum to n w/ 2 as the max number, for n from 1 to 100
  -- To do this, if possible, assume that the first number is 2, and go in order from
  -- from 1 to 100.  

-- How do I memoize this?
-- First, make a data structure that lets me look up a value with a tuple (Int, Int)
-- Then, do the memoization on it.

-- How many ways can I sum to 5 w/ a max of 3?
-- Take the number of ways I sum to 5 with a max of 2
-- Then how many more with a max of 3?
-- If it's new, one of the terms is a 3.
-- Then it's (2, 3)

memoizedNumWaysToSumTo100 :: (Int, Int) -> Integer
memoizedNumWaysToSumTo100 = ((map numWaysToSum [(a, b) | a <- [1..100], b <- [1..100]]) !!) . (\(a, b) -> (a - 1) * b + b - 1)
                                    where numWaysToSum (1, b) = 1
                                          numWaysToSum (m, b)
                                                | m > b      = memoizedNumWaysToSumTo100 (b, b)
                                                | m == b     = memoizedNumWaysToSumTo100 (m - 1, b) + 1
                                                | otherwise  = memoizedNumWaysToSumTo100 ((m - 1), b) + memoizedNumWaysToSumTo100 (m, (b - m))

--main = do
--    putStrLn . show $ memoizedNumWaysToSum (79, 80)

---- This non-memoized version is actually 4x faster!  Maybe b/c there's less overhead, and I think haskell
---- caches some results automatically.
--numWaysToSum :: (Int, Int) -> Int
--numWaysToSum (m, 0) = 1
--numWaysToSum (1, b) = 1
--numWaysToSum (m, b)
--    | m > b = numWaysToSum (b, b)
--    | otherwise = numWaysToSum ((m - 1), b) + numWaysToSum (m, (b - m))

--main = do
--    putStrLn . show $ numWaysToSum (99, 100)

memoizedFib :: Int -> Integer
memoizedFib = (map fib [0..] !!)
    where fib 0 = 0
          fib 1 = 1
          fib n = memoizedFib (n - 1) + memoizedFib (n - 2)

main = do
    putStrLn . show $ memoizedNumWaysToSumTo100 (100, 100)

--main = putStrLn . show $ memoizedFib 1000