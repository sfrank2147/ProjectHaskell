-- The reduced proper fractions for denominator d are just
-- numerators n that are relatively prime to d.
-- That's the totient function.
-- We can only calculate the totient function if we know the prime factorization of d.

-- Took 4 minutes 22 seconds on my macbook.

import qualified Data.Set as Set
import Prime

primes = primeList 1000000

recursiveTotient :: Int -> Int -> (Set.Set Int) -> Int
recursiveTotient 1 c _ = c
recursiveTotient n c s =
    let factor = head $ filter (\x -> n `mod` x == 0) primes
    in if Set.member factor s 
        then recursiveTotient (n `div` factor) (c * factor) (Set.insert factor s)
        else recursiveTotient (n `div` factor) (c * (factor - 1)) (Set.insert factor s)

totient :: Int -> Int
totient n = recursiveTotient n 1 Set.empty

answer = sum $ map totient [2..1000000]

main = putStrLn . show $ answer