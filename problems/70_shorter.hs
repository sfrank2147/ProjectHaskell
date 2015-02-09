import Data.Char
import qualified Data.Set as Set
import Prime
import qualified Debug.Trace as Trace
import Data.List.Extras.Argmax

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

-- how can i make this quicker?
-- we want n/phi(n) to be a minimum
--it would be great if we just had the same prime over and over again

-- strategy:
-- 1. generate all lists of primes whose product is smaller than 10000000
-- 2. for each of these lists, calculate phi
-- 3. filter out lists such that phi is a permutation of the value
-- 4. choose the one with the lowest ratio

primeFactorLists :: Int -> [[Int]]
primeFactorLists maxBound =
    let possiblePrimes = primeList maxBound
    -- create lists starting with each prime less than the maxBound
    in concat (map (primeFactorListsRecursive possiblePrimes maxBound) possiblePrimes)

primeFactorListsRecursive :: [Int] -> Int -> Int -> [[Int]]
-- return all lists of primes that multiply to less than maxBound
-- and which start with startingPrime
-- possiblePrimes is a helper list that contains primes we can use
primeFactorListsRecursive possiblePrimes maxBound startingPrime
    -- if the starting prime is greater than the maxBound, there's nothing we can do
    -- no lists of primes
    | maxBound < startingPrime = [[]]
    | possiblePrimes == []     = [[]]
    | otherwise                =
        -- for each prime greater than the starting prime, call recursively
        let nextMaxBound = maxBound `div` startingPrime
            actualPossiblePrimes = filter 
                    (\p -> (p >= startingPrime) && (p <= nextMaxBound)) possiblePrimes
            extensionLists = map (\p -> 
                    primeFactorListsRecursive actualPossiblePrimes nextMaxBound p) 
                    actualPossiblePrimes
            -- extensionLists is a list of list of lists of ints
            -- it has type [[[Int]]]
            -- flatten it once, so it's a list of lists of ints
            -- add startingPrime to the beginning of each of those lists
            -- and return that
            -- note that if actualPossiblePrimes is empty, we'll map over nothing
            -- in that case, we want to return [[startingPrime]]
            flattenedExtensionLists = if length actualPossiblePrimes > 0 then  concat extensionLists else [[]]--type [[Int]]
        in map (\ps -> startingPrime:ps) flattenedExtensionLists

calculatePhiFromPrimeFactors :: [Int] -> Int
--what is the phi value from the list of prime factors?
calculatePhiFromPrimeFactors = calculatePhiFromPrimeFactorsRecursive Set.empty 1

calculatePhiFromPrimeFactorsRecursive :: (Set.Set Int) -> Int -> [Int] -> Int
calculatePhiFromPrimeFactorsRecursive _ curr [] = curr
calculatePhiFromPrimeFactorsRecursive primeSet curr (p:ps) =
    -- if we've already seen the prime before, multiply by the full prime
    -- otherwise, multiply by (p - 1) and add the prime to the set we've seen
    if (Set.member p primeSet)
        then calculatePhiFromPrimeFactorsRecursive primeSet (curr * p) ps
        else calculatePhiFromPrimeFactorsRecursive (Set.insert p primeSet) (curr * (p - 1)) ps

answer :: Int -> Int
answer maxBound =
    let possibleFactorSets = primeFactorLists maxBound --list of lists
        phiVals = map calculatePhiFromPrimeFactors possibleFactorSets --list of Ints
        numAndPhiList = zip (map (foldl (*) 1) possibleFactorSets) phiVals
        permNumAndPhi = filter (\(n, p) -> arePermutations n p) numAndPhiList
    in fst $ argmin (\(n, p) -> (fromIntegral n) / (fromIntegral p)) permNumAndPhi

main = putStrLn . show $ answer 100000