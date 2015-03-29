-- First, generate a list of all the primes below the cap (say 1000000?)
-- Let P(n) be the number of ways I can write n as a sum of primes.
-- P(1) = 0, b/c I can't write 1 as a sum of primes.
-- To calculate P(n), it's sum_{primes p s.t. p <= n}[P(n - p)]
import Prime
import qualified Data.Map as Map

primes = primeList 1000

numPrimeSumsWithMaxPrime :: Int -> Int -> Int
numPrimeSumsWithMaxPrime 0 _ = 1
numPrimeSumsWithMaxPrime 1 _ = 0
numPrimeSumsWithMaxPrime n m
    | m > n = numPrimeSumsWithMaxPrime n n
    | otherwise = sum $ map (\p -> numPrimeSumsWithMaxPrime (n - p) p) (takeWhile ((>=) m) primes)

-- Memoized version of the above function
-- Traverse the values of n - for each n, solve for p from 1 to target.
memoizedNumPrimesWithMaxPrimeMap :: Int -> Int -> Int -> (Map.Map (Int, Int) Int) -> (Map.Map (Int, Int) Int)
memoizedNumPrimesWithMaxPrimeMap n l target m =
    let r =
            if n == 0 then 1 else
            if n == 1 then 0 else
            if l > n then m Map.! (n, n) else
            sum $ map (\p -> m Map.! (n - p, p)) (takeWhile ((>=) l) primes)
        nextMap = Map.insert (n, l) r m
    in if l < target then memoizedNumPrimesWithMaxPrimeMap n (l + 1) target nextMap else
       if n < target then memoizedNumPrimesWithMaxPrimeMap (n + 1) 1 target nextMap else
       nextMap

--numPrimeSums n = numPrimeSumsWithMaxPrime n n
numPrimeSums n = (memoizedNumPrimesWithMaxPrimeMap 0 1 n Map.empty) Map.! (n, n)

answer =
    let countMap = memoizedNumPrimesWithMaxPrimeMap 0 1 1000 Map.empty
    in head $ filter (\x -> countMap Map.! (x, x) > 5000) [2..]

main = putStrLn . show $ answer