import qualified Data.Set as Set

properDivisors :: Int -> [Int]
properDivisors x =
    let half = floor ((fromIntegral x) / 2)
    in filter (\y -> x `mod` y == 0) [1..half]

isAbundant :: Int -> Bool
isAbundant x = sum (properDivisors x) > x

smallAbundantNumbers = filter isAbundant [1..28123]

sumsOfAbundants = [x + y | x <- smallAbundantNumbers, y <- smallAbundantNumbers]

sumsOfAbundantsSet = Set.fromList sumsOfAbundants

notSumOfAbundants = filter (flip Set.notMember sumsOfAbundantsSet) [1..28123]

main = putStrLn . show $ sum notSumOfAbundants