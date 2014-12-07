import Prime

main = putStrLn $ show longestRepetend

longestRepetend :: Int
--return the largest prime s.t. 10 is a primitive root
longestRepetend = 
    let possiblePrimes = [2..1000]
        primeList = filter isPrime possiblePrimes
        tenPrimitivePrimes = filter tenIsPrimitiveRoot primeList
    in maximum tenPrimitivePrimes

tenIsPrimitiveRoot :: Int -> Bool
--returns true if 10 is a primitive root of the prime
--p must be a prime
tenIsPrimitiveRoot p =
    let powersList = [exponentMod 10 y p | y <- [1..(p-1)]]
        numOnes = length $ filter (==1) powersList
    in  numOnes == 1

exponentMod :: Int -> Int -> Int -> Int
exponentMod base 0 _ = 1
exponentMod base 1 p = base `mod` p
exponentMod base n p = (base * (exponentMod base (n-1) p)) `mod` p