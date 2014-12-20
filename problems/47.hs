import Prime
import Data.List
import Data.Set as Set
import Data.Maybe

smallestPrimeFactor :: Int -> Maybe Int
smallestPrimeFactor 1 = Nothing
smallestPrimeFactor x = find (\p -> x `mod` p == 0) [2..x]

primeFactors :: Int -> (Set.Set Int)
primeFactors x
    | x <= 1 = Set.empty
    | otherwise = primeFactorsRecursive x Set.empty

primeFactorsRecursive :: Int -> (Set.Set Int) -> (Set.Set Int)
primeFactorsRecursive 1 currSet = currSet
primeFactorsRecursive x currSet =
    let nextFactor = smallestPrimeFactor x
    in primeFactorsRecursive (x `div` (fromMaybe 1 nextFactor)) (Set.insert (fromMaybe 1 nextFactor) currSet)


validSolution :: Int -> Bool
validSolution x =
    Set.size (primeFactors x) == 4 && Set.size (primeFactors $ x + 1) == 4 && Set.size (primeFactors $ x + 2) == 4 && Set.size (primeFactors $ x + 3) == 4

main = do
    let answer = find validSolution [2..]
    putStrLn $ show answer