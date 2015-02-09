import Data.Char
import qualified Data.Set as Set
import Prime
import qualified Debug.Trace as Trace
import Data.List.Extras.Argmax

---- is it possible that the answer factors into a single prime (with multiplicity)?
--phiFromSinglePrime :: Int -> Int -> Int
--phiFromSinglePrime p n = (p - 1) * p^(n-1)

arePermutations :: Int -> Int -> Bool
--return true if the ints' representations are permutations of each other
arePermutations x y = all (\d -> countDigit d x == countDigit d y) [0..9]

countDigit :: Int -> Int -> Int
--count the number of times digit d appears in number n
countDigit d n 
    | d < 0 = 0
    | d > 9 = 0
    | otherwise =
        let digitChar = intToDigit d
        in length $ filter (\x -> x == digitChar) (show n)

answer :: Int
answer =
    let possiblePrimes = primeList 10000000
        possiblePairs = [(p, q) | p <- possiblePrimes, q <- takeWhile (\x -> x * p <= 10000000) possiblePrimes]
        validPairs = filter (\(p, q) -> arePermutations (p * q) ((p - 1) * (q - 1))) possiblePairs
        bestPair = argmin (\(p, q) -> fromIntegral(p * q) / fromIntegral((p - 1) * (q - 1))) validPairs
    in (fst bestPair) * (snd bestPair)

main = putStrLn . show $ answer