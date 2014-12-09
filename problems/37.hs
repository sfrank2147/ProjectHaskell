import qualified Data.Set as Set
import Prime

----parameters: current number, maxNumber, current set
----returns a set of all the truncatable primes up to maxNumber
--truncatablePrimes :: Int -> Int -> (Set.Set Int) -> (Set.Set Int)
--truncatablePrimes curr maxNumber currSet
--    | curr > maxNumber = currSet
--    --if the current number isn't prime, move on
--    | not (isPrime curr) = truncatablePrimes (curr + 1) maxNumber currSet
--    --if the current number is two digits, check if each digit is prime
--    | length (show curr) == 2 && isPrime (curr `div` 10) && isPrime (curr `mod` 10) = truncatablePrimes (curr + 1) maxNumber (Set.insert curr currSet)
--    | length (show curr) == 2 = truncatablePrimes (curr + 1) maxNumber currSet
--    --if the current number is more than two digits, check if both subcomponents are in the set
--    | Set.member (read (tail (show curr))) currSet && Set.member (curr `div` 10) currSet = truncatablePrimes (curr + 1) maxNumber (Set.insert curr currSet)
--    | otherwise = truncatablePrimes (curr + 1) maxNumber currSet

--main = do
--    let primes = truncatablePrimes 10 5000 (Set.empty)
--    putStrLn $ show primes

leftTruncatable :: Int -> Int -> (Set.Set Int) -> (Set.Set Int)
leftTruncatable curr maxNum currSet
    | curr > maxNum = currSet
    --if curr isn't prime, move on
    | not (isPrime curr) = leftTruncatable (curr + 1) maxNum currSet
    --if it's two digits, check if whole thing is prime, and if right digit is prime
    | length (show curr) == 2 && isPrime (curr `mod` 10) = leftTruncatable (curr + 1) maxNum (Set.insert curr currSet)
    --otherwise, check if the rightmost half is in the set already
    | Set.member (read (tail (show curr))) currSet = leftTruncatable (curr + 1) maxNum (Set.insert curr currSet)
    | otherwise = leftTruncatable (curr + 1) maxNum currSet

rightTruncatable :: Int -> Int -> (Set.Set Int) -> (Set.Set Int)
rightTruncatable curr maxNum currSet
    | curr > maxNum = currSet
    --if curr isn't prime, move on
    | not (isPrime curr) = rightTruncatable (curr + 1) maxNum currSet
    --if it's two digits, check if whole thing is prime, and if left digit is prime
    | length (show curr) == 2 && isPrime (curr `div` 10) = rightTruncatable (curr + 1) maxNum (Set.insert curr currSet)
    --otherwise, check if the leftmost half is in the set already
    | Set.member (curr `div` 10) currSet = rightTruncatable (curr + 1) maxNum (Set.insert curr currSet)
    | otherwise = rightTruncatable (curr + 1) maxNum currSet

bothTruncatable :: Int -> (Set.Set Int)
bothTruncatable maxNum = Set.intersection (leftTruncatable 10 maxNum Set.empty) (rightTruncatable 10 maxNum Set.empty)

main = putStrLn $ show (bothTruncatable 1000000)