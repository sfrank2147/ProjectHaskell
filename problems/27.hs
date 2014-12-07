import Prime

main = do
    putStrLn $ show (mostConsecutivePrimes 999 999)

--function to find which quadratic in the given range
--has the most consecutive primes
mostConsecutivePrimes :: Int -> Int -> (Int, Int)
mostConsecutivePrimes maxA maxB =
    let candidates = [(a,b) | a <- [(-maxA)..maxA], b <- [(-maxB)..maxB]]
    in snd $ maximum $ map (\(a,b) -> ((numConsecutivePrimes a b), (a,b))) candidates


--function to determine the number of consecutive primes
--for the function n^2 + an + b
--for n starting at 0
numConsecutivePrimes :: Int -> Int -> Int
numConsecutivePrimes a b =
    let partialQuadratic = quadratic a b
        primeList = takeWhile isPrime $ map (abs . partialQuadratic) [0..]
    in  length primeList

--function to calculate n^2 + an + b
quadratic :: Int -> Int -> Int -> Int
quadratic a b n = n * n + a * n + b