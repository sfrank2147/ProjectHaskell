import Data.Char
import qualified Data.Set as Set

type Chain = [Int]

containsCycle :: Chain -> Bool
containsCycle c = (Set.size . Set.fromList $ c) < length c

noCycle = not . containsCycle

factorial :: Int -> Int
factorial 0 = 1
factorial x = product [1..x]

transition :: Int -> Int
transition = sum . map (factorial . digitToInt) . show

firstNElements :: Int -> Int -> Chain
firstNElements n x = take n $ iterate transition x

firstSixtyElements = firstNElements 60

validSolution = noCycle . firstSixtyElements

finalCount = length $ filter validSolution [1..999999]

main = putStrLn . show $ finalCount