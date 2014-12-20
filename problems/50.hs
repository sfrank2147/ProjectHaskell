import Prime
import qualified Data.Set as Set

--get a list of primes under 1000000
--make a set out of that list
--for each starting prime, for each consecutive number until a million, if the
--sum is a prime, add (length of sum, prime) to a third list storing results
--take the maximum of that list

consecutivePrimes :: (Int, Int)
consecutivePrimes = 
    let primeList = filter isPrime [2..1000000]
        primeSet = Set.fromList primeList
        numPrimes = length primeList
        consPrimeList = map (\x -> mostConsPrimes x primeList primeSet) [0..(numPrimes - 1)]
    in maximum consPrimeList

mostConsPrimes :: Int -> [Int] -> (Set.Set Int) -> (Int, Int)
--mostConsPrimes x primes is (num, total), where num consecutive
--primes, starting with x, add up to total, a prime
mostConsPrimes p primes primeSet =
    let consSums = takeWhile (\(x,y) -> y <= 1000000) $ map (\num -> (num, listSum primes p num)) [1..]
        primeSums = filter (\(x,y) -> Set.member y primeSet) consSums
    in maximum primeSums


listSum :: [Int] -> Int -> Int -> Int
listSum list start num = sum $ take num $ drop (start - 1) list

main = putStrLn $ show $ consecutivePrimes