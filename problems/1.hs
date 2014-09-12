main = putStrLn $ show sumOfMultiples

sumOfMultiples :: Int
sumOfMultiples = sum $ filter validMultiple [1..999]

validMultiple :: Int -> Bool
validMultiple x = (x `mod` 3 == 0) || (x `mod` 5 == 0)