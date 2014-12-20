isPerfectSquare :: Int -> Bool
--return true if a number is a perfect square
isPerfectSquare x =
    let squareRootEstimate = floor . sqrt . fromIntegral $ x
    in (squareRootEstimate ^ 2) == x

isPentagonal :: Int -> Bool
isPentagonal x = (isPerfectSquare $ 1 + 24 * x) && ((floor . sqrt . fromIntegral $ 1 + 24 * x) `mod` 6 == 5)

--hexagonal numbers: n*(2n-1) is x
--2n^2 - n - x = 0
--n = (1 +/- sqrt(1 + 8*x))/4
--to be hexagonal, must have 8*x+1 be a perfect square
--and must have sqrt(1 + 8*x) `mod` 4 == 3
isHexagonal :: Int -> Bool
isHexagonal x = (isPerfectSquare $ 1 + 8 * x) && ((floor . sqrt . fromIntegral $ 1 + 8 * x) `mod` 4 == 3)

main = do
    let triangularNumList = [(n * (n + 1)) `div` 2 | n <- [286..]]
        validNums = filter (\x -> isPentagonal x && isHexagonal x) triangularNumList
    putStrLn . show $ head validNums