import Prime

main = do
    let targetNum = 600851475143
        squareRoot = floor $ sqrt (fromIntegral targetNum)
        possibleDivisors = [squareRoot, (squareRoot - 1)..]
        divisors = filter (\x -> targetNum `mod` x == 0) possibleDivisors
        largestPrimeDivisor = head (filter isPrime divisors)
    putStrLn (show largestPrimeDivisor)