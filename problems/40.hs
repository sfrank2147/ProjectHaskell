import Data.Char

digitn :: Int -> Int
digitn x = digitnRecursive 1 1 x

digitnRecursive :: Int -> Int -> Int -> Int
digitnRecursive currNum currDigit targetDigit
    | currDigit + length (show currNum) - 1 < targetDigit = digitnRecursive (currNum + 1) (currDigit + (length $ show currNum)) targetDigit
    | otherwise = digitToInt $ (show currNum) !! (targetDigit - currDigit)


main = putStrLn . show $ product (map digitn [1,10,100,1000,10000,100000,1000000])