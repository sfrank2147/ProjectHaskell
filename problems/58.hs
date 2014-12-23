import Prime
import Data.Ratio

diagonalEntries :: Int -> [Int]
--what are the diagonal entries for layer x?
diagonalEntries x =
    let previousMax = 1 + (sum $ map (\y -> 4 * 2 * y) [1..(x - 1)])
    in map (+ previousMax) [2*x, 4*x, 6*x, 8*x]

numPrimeDiagonalEntries :: Int -> Int
--how many prime diagonal entries in layer x?
numPrimeDiagonalEntries = length . filter isPrime . diagonalEntries

numDiagonals :: Int -> Int
--how many diagonal entries up through layer x?
numDiagonals x = 4 * x + 1

solution :: Int
solution =
    let primeDiagonalEntryCounts = map numPrimeDiagonalEntries [1..]
        accumulatedDiagonalPrimeCounts = scanl (+) 0 primeDiagonalEntryCounts
        ratios = map (\x -> (accumulatedDiagonalPrimeCounts !! x) % (numDiagonals x)) [1..]
    in head $ filter (\idx -> ratios !! (idx - 1) < 0.10) [1..]

main = putStrLn . show $ 2 * solution + 1