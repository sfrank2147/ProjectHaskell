import Data.Char

pandigitals :: Int -> [Int]
--return a list of pandigitals of length n
pandigitals n =
    let nStr = map intToDigit [1..n]
        pandigitalStrings = perms nStr
    in map read pandigitalStrings

perms :: String -> [String]
perms str
    | length str == 1 = [str]
    | otherwise = foldl (++) [] [permsStartingWithIndex str n | n <- [0..(length str - 1)]]

permsStartingWithIndex :: String -> Int -> [String]
permsStartingWithIndex str n
    | n < 0 = []
    | n >= length str = []
    | otherwise =
        let start = str !! n --start is a char
            restOfString = (let (firstHalf, secondHalf) = splitAt n str in firstHalf ++ tail secondHalf)
            permsOfRest = perms restOfString
        in [start:perm | perm <- permsOfRest]

isPrime :: Int -> Bool
--return True if x is prime
isPrime x =
    let possibleFactors = takeWhile (\y -> y * y <= x) [2..]
    in all (\y -> x `mod` y /= 0) possibleFactors

largestNDigitPandigital :: Int
largestNDigitPandigital =
    let pandigitalList = foldl (++) [] [pandigitals n | n <- [1..7]]
        oddPandigitals = filter (\x -> x `mod` 2 == 1) pandigitalList
        primePandigitals = filter isPrime oddPandigitals
    in maximum primePandigitals

main = do 
    putStrLn $ show largestNDigitPandigital
