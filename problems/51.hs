import Prime
import Data.Char
import qualified Data.Set as Set

primeNumbers = Set.fromList $ primeList 999999

replaceAllDigits :: Int -> [Int] -> Int -> Int
--replaceAllDigits num digitsToReplace val replaces all the specified digits in num
replaceAllDigits num digitsToReplace val =
    let numStr = show num
        replacedNumStr = map (\dig -> if dig `elem` digitsToReplace then (intToDigit val) else numStr!!dig) [0..(length numStr - 1)]
    in read replacedNumStr

allReplacements :: Int -> [Int] -> [Int]
--create a list of all the replacements from replacing digits in the number
allReplacements num digitsToReplace = map (replaceAllDigits num digitsToReplace) [0..9]

validSolution :: Int -> [Int] -> Bool
--is the solution valid
validSolution num digitsToReplace =
    let replacements = allReplacements num digitsToReplace
        primeReplacements = filter (\x -> Set.member x primeNumbers) replacements
    in (length primeReplacements >= 8)


sizeThreeSubsets :: Int -> [[Int]]
--returns a list of subsets of size n of the numbers from 0 to max
sizeThreeSubsets max = [[a,b,c] | a <- [0..(max - 3)], b <- [(a+1)..(max - 2)], c <-[(b+1)..(max - 1)]]

sizeFourSubsets :: Int -> [[Int]]
sizeFourSubsets max = [[a,b,c,d] | a <- [0..(max - 4)], b <- [(a+1)..(max - 3)], c <- [(b+1)..(max - 2)], d <- [(c+1)..(max - 1)]]


solution :: [(Int, [Int])]
solution =
    let possibleBaseSolutions = [100000..999999]
        possibleCombos = [(num, digits) | num <- possibleBaseSolutions, digits <- (sizeThreeSubsets 5)]
        validSolutions = filter (\(num, digits) -> validSolution num digits) possibleCombos
    in validSolutions


main = putStrLn . show $ solution