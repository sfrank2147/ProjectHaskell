import Data.Char

--return x!
factorial :: Int -> Int
factorial 1 = 1
factorial 0 = 1
factorial x = x * (factorial (x - 1))

--let the number be n digits long
--we need that n * 9! >= 10^(n-1)
--log_10(9!) + log_10(n) >= n-1
--5.56 + log_10(n) >= n-1
--n must be 7 or less
--alternately, floor(log_10(N)) * 9! >= N
--this fails for N greater than 3000000

--how can we check if N is written that way, just sum all its digits?

--return the sum of a number's digits' factorials
sumOfDigitFactorials :: Int -> Int
sumOfDigitFactorials num =
    let digitChars = show num
        digits = map digitToInt digitChars
    in sum $ map factorial digits

isSumOfFactorials :: Int -> Bool
isSumOfFactorials x = (sumOfDigitFactorials x) == x

main = do
    let answer = sum $ filter isSumOfFactorials [10..3000000]
    putStrLn $ show answer
