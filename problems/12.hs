main = putStrLn $ show solution

triangularNums :: [Int]
triangularNums = scanl (+) 1 [2..]

numDivisors :: Int -> Int
numDivisors x =
    let squareRootRoundedDown = floor (sqrt (fromIntegral x))
        possibleSmallDivisors = [1..squareRootRoundedDown]
        smallDivisors = filter (\y -> x `mod` y == 0) possibleSmallDivisors
        squareRootSubtraction = if squareRootRoundedDown ^ 2 == x then 1 else 0
    in 2 * (length smallDivisors) - squareRootSubtraction


solution :: Int
solution = head $ filter (\x -> numDivisors x > 500) triangularNums