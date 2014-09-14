import Data.Char

main = putStrLn . show $ sum [digitToInt x :: Int| x <- (show (2^1000))]