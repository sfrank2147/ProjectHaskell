import Data.Char

factorial :: Int -> Integer
factorial 1 = 1
factorial x = (toInteger x) * (factorial (x - 1))

hundredFactorial = factorial 100
hFactorialString = show hundredFactorial
hFactorialDigitList = [digitToInt dig | dig <- hFactorialString]

main = putStrLn . show $ sum hFactorialDigitList