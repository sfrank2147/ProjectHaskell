module Polygonal where

--steal some functions from problems 42, 44, and 45
--found using grep

isPerfectSquare :: Int -> Bool
--return true if a number is a perfect square
isPerfectSquare x =
    let squareRootEstimate = floor . sqrt . fromIntegral $ x
    in (squareRootEstimate ^ 2) == x

isTriangular :: Int -> Bool
--returns true if x is a triangle number
--consider n*(n+1)/2 is x
--solve with quadratic formula
--n is -1 +/- sqrt(1 + 8x)/2
--so x is a triangle num if x is greater than 0
--and if 8x+1 is a perfect square
isTriangular x = (x > 0) && (isPerfectSquare $ 8*x + 1)

isPentagonal :: Int -> Bool
isPentagonal x = (isPerfectSquare $ 1 + 24 * x) && ((floor . sqrt . fromIntegral $ 1 + 24 * x) `mod` 6 == 5)

--hexagonal numbers: n*(2n-1) is x
--2n^2 - n - x = 0
--n = (1 +/- sqrt(1 + 8*x))/4
--to be hexagonal, must have 8*x+1 be a perfect square
--and must have sqrt(1 + 8*x) `mod` 4 == 3
isHexagonal :: Int -> Bool
isHexagonal x = (isPerfectSquare $ 1 + 8 * x) && ((floor . sqrt . fromIntegral $ 1 + 8 * x) `mod` 4 == 3)

--heptagonal numbers are of the form n * (5n - 3) / 2 is x
--so then 5n^2 - 3n = 2 x
--5n^2 - 3n - 2x = 0
--n is of the form (3 +/- sqrt(9 + 40x)) / 10
--so, we must have 9 + 40x is a perfect square
--and sqrt(9 + 40x) `mod` 10 == 7
isHeptagonal :: Int -> Bool
isHeptagonal x = (isPerfectSquare $ 9 + 40 * x) && ((floor . sqrt . fromIntegral $ 9 + 40 * x) `mod` 10 == 7)

--octagonal numbers of the form n(3n - 2)
--3n^2 - 2n = x
--3n^2 - 2n - x = 0
--n = (2 +/- sqrt(4 + 12x))/6
--so need 4 + 12x to be a perfect square
--and need sqrt(4 + 12x) to be congruent to 4 mod 6
isOctagonal :: Int -> Bool
isOctagonal x = (isPerfectSquare $ 4 + 12 * x) && ((floor . sqrt . fromIntegral $ 4 + 12 * x) `mod` 6 == 4)
