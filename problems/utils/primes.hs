module Prime where

primeList :: Int -> [Int]
--equals a list of all the prime numbers less than the argument
--calculated using sieve of eratasthones
primeList 1 = []
primeList highestNum =
    let candidatesList = [2..highestNum]
    in runSieve candidatesList

runSieve :: [Int] -> [Int]
--run the sieve on the list we have so far
runSieve [] = []
runSieve (x:xs) 
    | length xs == 0                  = [x]
    | (fromIntegral x) > sqrt (fromIntegral (last xs)) = x:xs
    | otherwise = x:(runSieve $ filter (\y -> y `mod` x /= 0) xs)

isPrime :: Int -> Bool
isPrime num
    | num < 2 = False
    | otherwise =
        let upperBound = floor (sqrt (fromIntegral num))
            divisors = fmap (\x -> num `mod` x == 0) [2..upperBound]
        in (length $ filter (==True) divisors) == 0