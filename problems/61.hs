--steal some functions from problems 42, 44, and 45
--found using grep

import Data.List
import Data.Maybe
import qualified Data.Set as Set

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


--note: all of the above functions can now be exported from utils/polygonal.hs
cyclicPair :: Int -> Int -> Bool
--assume these are 4-digit numbers
--do the last 2 digits of x match the first 2 digits of y?
cyclicPair x y = (drop 2 $ show x) == (take 2 $ show y)

thirdDigitNonZero :: Int -> Bool
thirdDigitNonZero x = (show x) !! 2 /= '0'

triangles :: [Int]
squares :: [Int]
pentagonals :: [Int]
hexagonals :: [Int]
heptagonals :: [Int]
octagonals :: [Int]

triangles = filter thirdDigitNonZero . filter isTriangular $ [1000..9999]
squares = filter thirdDigitNonZero . filter isPerfectSquare $ [1000..9999]
pentagonals = filter thirdDigitNonZero . filter isPentagonal $ [1000..9999]
hexagonals = filter thirdDigitNonZero . filter isHexagonal $ [1000..9999]
heptagonals = filter thirdDigitNonZero . filter isHeptagonal $ [1000..9999]
octagonals = filter thirdDigitNonZero . filter isOctagonal $ [1000..9999]

type Permutation = [[Int]]

validCycle :: Permutation -> Maybe (Int, Int, Int, Int, Int, Int)
validCycle perm =
    let twoCycles = [(a, b) | a <- (perm !! 0), b <- (perm !! 1), a /= b, cyclicPair a b] --all the valid pairs of two
        threeCycles = [(a, b, c) | (a, b) <- twoCycles, c <- (perm !! 2), cyclicPair b c]
        fourCycles = [(a, b, c, d) | (a, b, c) <- threeCycles, d <- (perm !! 3), cyclicPair c d]
        fiveCycles = [(a, b, c, d, e) | (a, b, c, d) <- fourCycles, e <- (perm !! 4), cyclicPair d e]
        sixCycles = [(a, b, c, d, e, f) | (a, b, c, d, e) <- fiveCycles, f <- (perm !! 5), cyclicPair e f]
        solutions = filter allUnique . filter (\(a, b, c, d, e, f) -> cyclicPair f a) $ sixCycles
            where allUnique (a, b, c, d, e, f) = length (Set.toList . Set.fromList $ [a, b, c, d, e, f]) == 6
    in if length solutions > 0 then Just (head solutions) else Nothing

sumDigits :: (Int, Int, Int, Int, Int, Int) -> Int
sumDigits (a, b, c, d, e, f) = sum [a, b, c, d, e, f]

main = do
    let perms = permutations [triangles, squares, pentagonals, hexagonals, heptagonals, octagonals]
        solutions = catMaybes $ map validCycle perms
        numSums = map sumDigits solutions
    putStrLn . show $ numSums