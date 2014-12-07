main = putStrLn . show $ (downRightSum 1001 + upRightSum 1001 - 1)


--upper left to bottom right sum
--start with 1
-- for 2, it's (2-1)^2 + 1*2 and (2-1)^2 + 3*2
-- for 4, it's (4-1)^2 + 1*4 and (4-1)^2 + 3*4
downRightSum :: Int -> Int
downRightSum dim = 
    let evens = filter (\x -> x `mod` 2 == 0) [1..dim]
    in sum [2*(ev - 1)^2 + 4*ev | ev <- evens] + 1

--lower left to upper right
--start with 1
--for 3, it' 3^2 on the upper right, and 3^2 - 2*(3-1) on the bottom left
--for 5, it's 5^2 on the upper right, and 5^2 - 2*(5-1) on the bottom left
upRightSum :: Int -> Int
upRightSum 1 = 1
upRightSum dim =
    let odds = filter (\x -> x `mod` 2 == 1) [2..dim]
    in sum [2 * x^2 - 2 * (x - 1) | x <- odds] + 1