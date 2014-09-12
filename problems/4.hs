main = putStrLn $ show largestPalProduct

largestPalProduct :: Int
largestPalProduct = maximum $ filter isPalindromeNum $ [x * y | x <- [100..999], y <- [100..999]]

isPalindromeNum :: Int -> Bool
isPalindromeNum x =
    let xString = show x
    in reverse xString == xString