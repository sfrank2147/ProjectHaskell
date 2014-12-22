module Pandigital where

import Data.Char

pandigitals :: Int -> [Int]
--return a list of pandigitals of length n
pandigitals n =
    let nStr = map intToDigit [1..n]
        pandigitalStrings = perms nStr
    in map read pandigitalStrings

pandigitalsInRange :: Int -> Int -> [Int]
--return a list of pandigitals using digits from a to b
pandigitalsInRange a b =
    let nStr = map intToDigit [a..b]
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