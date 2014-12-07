--7*9^5 only has 6 digits, so any number with 7 digits can't be written as the sum of fifth powers of its digits
--6*9^5 = 354294, so that's the upper bound for a 6-digit number that can be written as the sum of its digits

import Data.Char

sumOfDigits :: Int -> Int
sumOfDigits x =
    let digitList = map digitToInt (show x) --array of ints
    in sum [y^5 | y <- digitList]

isSumOfDigits :: Int -> Bool
isSumOfDigits x = sumOfDigits x == x

main = putStrLn . show $ sum (filter isSumOfDigits [2..354294])