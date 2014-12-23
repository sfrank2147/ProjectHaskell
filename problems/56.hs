import Data.Char
import Data.List.Extras.Argmax

digitalSum :: Integer -> Int
digitalSum num = foldl (+) 0 . map digitToInt . show $ num

exponents :: [Integer]
exponents = [a^b | a <- [1..100], b <- [1..100]]

main = do
    let maxDigitalSum = maximum . map digitalSum $ exponents
    putStrLn . show $ maxDigitalSum