import Data.Digits
import Data.Char

--return true if both the binary and decimal representations are palindromes
isDecAndBinaryPalindrome :: Int -> Bool
isDecAndBinaryPalindrome x =
    x `mod` 2 == 1 --even numbers can't be palindromes base 2, b/c no leading 0s
    && isDecimalPalindrome x
    && isBaseTwoPalindrome x

--return true if the decimal representation is a palindrome
isDecimalPalindrome :: Int -> Bool
isDecimalPalindrome = isPalindrome . show

isBaseTwoPalindrome :: Int -> Bool
isBaseTwoPalindrome x = isPalindrome $ (map intToDigit (digits 2 x))


isPalindrome :: String -> Bool
isPalindrome str =
    let strLength = length str
    in all (\idx -> str!!idx == str!!(strLength - idx - 1)) [0..(strLength `div` 2)]

main = do
    let answer = sum $ filter isDecAndBinaryPalindrome [1..999999]
    putStrLn $ show answer