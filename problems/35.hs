import Data.Char

isPrime :: Int -> Bool
isPrime x
    | x < 2 = False
    | otherwise =
        let possibleFactors = takeWhile (\p -> p * p <= x) [2..]
            factors = filter (\p -> x `mod` p == 0) possibleFactors
        in  length factors == 0

isOdd x = x `mod` 2 == 1

--return all the cycles of an int
cycles :: Int -> [Int]
cycles x =
    let xString = show x
        numDigits = length xString
        cycleStrings = [drop n xString ++ take n xString | n <- [0..(numDigits - 1)]]
    in map read cycleStrings

allCyclesPrime :: Int -> Bool
allCyclesPrime 2 = True
allCyclesPrime x = 
    all isOdd (map digitToInt (show x))
    && (length $ filter isPrime (cycles x)) == (length $ show x)

main = putStrLn $ show (length (filter allCyclesPrime [2..999999]))