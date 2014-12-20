--idea: generate a list of pentagonal numbers (first 1000 maybe?)
--create a list of tuples of (non-identical) pairs of pentagonal numbers
--sort by difference
--return the first pair where the sum and difference are also pentagonal

--to create list of pentagonal numbers, just generate with formula
--to test if a number if pentagonal, need 1+24x to be a perfect square and need sqrt(1+24x) `mod` 6 == 5
--to speed things 


isPerfectSquare :: Int -> Bool
--return true if a number is a perfect square
isPerfectSquare x =
    let squareRootEstimate = floor . sqrt . fromIntegral $ x
    in (squareRootEstimate ^ 2) == x

isPentagonal :: Int -> Bool
isPentagonal x = (isPerfectSquare $ 1 + 24 * x) && ((floor . sqrt . fromIntegral $ 1 + 24 * x) `mod` 6 == 5)

pentagonalNum :: Int -> Int
pentagonalNum n = (n * (3 * n - 1)) `div` 2

validTuple :: (Int, Int) -> Bool
validTuple (x,y) = (isPentagonal (y - x)) && (isPentagonal (x + y))

main = do
    --tests
    putStrLn $ show $ isPerfectSquare 16
    putStrLn $ show $ not . isPerfectSquare $ 17
    putStrLn $ show $ isPentagonal 51
    putStrLn $ show $ not . isPentagonal $ 50
    putStrLn $ show $ (pentagonalNum 4 == 22)

    --actual work
    let pentagonalTuples = [(pentagonalNum x, pentagonalNum y) | x <- [1..4000], y <- [(x+1)..4000]]
        validTuples = filter validTuple pentagonalTuples
    putStrLn $ show validTuples