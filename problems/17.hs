import Data.Char

main = putStrLn . show $ sum [numberWordLength x | x <- [1..1000]]

digitWordLengths :: [Int]
digitWordLengths = [0,3,3,5,4,4,3,5,5,4]

teenWordLengths :: [Int]
teenWordLengths = [3, 6, 6, 8, 8, 7, 7, 9, 8, 8]

tenPrefixLengths :: [Int]
tenPrefixLengths = [0, 0, 6, 6, 5, 5, 5, 7, 6, 6]

numberWordLength :: Int -> Int
numberWordLength x
    | x < 10 = digitWordLengths !! x --just a one digit number
    | x < 20 = teenWordLengths !! (x - 10)
    | x < 100 = tenPrefixLengths !! (floor ( (fromIntegral x) / 10)) + (digitWordLengths !! (x `mod` 10))
    | x `mod` 100 == 0 && x < 1000 = digitWordLengths !! (floor ((fromIntegral x) / 100)) + 7
    | x < 1000 = digitWordLengths !! (floor ((fromIntegral x) / 100)) + 7 + 3 + (numberWordLength (x `mod` 100))
    | x == 1000 = 11