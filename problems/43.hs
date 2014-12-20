import Pandigital
import Data.List.Utils

subNumber :: Int -> Int -> Int -> Int
--returns the "substring" number from a to b, last index exclusive
--e.g., subNumber 6789 1 3 returns 78
subNumber x a b =
    let numStr = show x
        subStr = take (b - a) . drop a $ numStr
    in read subStr

specialNum :: Int -> Bool
specialNum x =
    (subNumber x 1 4) `mod` 2 == 0
    && (subNumber x 2 5) `mod` 3 == 0
    && (subNumber x 3 6) `mod` 5 == 0
    && (subNumber x 4 7) `mod` 7 == 0
    && (subNumber x 5 8) `mod` 11 == 0
    && (subNumber x 6 9) `mod` 13 == 0
    && (subNumber x 7 10) `mod` 17 == 0

main = do
    let allPans = pandigitalsInRange 0 9
        validPans = filter specialNum allPans
    putStrLn . show $ sum validPans