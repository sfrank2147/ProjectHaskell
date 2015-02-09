import Prime
import qualified Data.Set as Set

largePrimeList = primeList 99999999
largePrimeSet = Set.fromList largePrimeList

concatNums :: Int -> Int -> Int
concatNums x y = read (show x ++ show y)

mergeLists :: [Int] -> [Int] -> [Int]
--merge lists of primes, deleting duplicates
mergeLists xs ys = Set.toList $ Set.fromList (xs ++ ys)

validPair :: Int -> Int -> Bool
validPair a b = Set.member (concatNums a b) largePrimeSet && Set.member (concatNums b a) largePrimeSet

validList :: [Int] -> Bool
validList ps = all (\(x,y) -> validPair x y) [(x, y) | x <- ps, y <- ps, x /= y]

pairList :: [[Int]]
pairList =
    let ps = primeList 9999
    in [[a, b] | a <- ps, b <- ps, a < b, validPair a b]

threeOrFourList :: [[Int]]
threeOrFourList =
    let allMerged = [mergeLists as bs | as <- pairList, bs <- pairList, as < bs]
    in  filter validList allMerged

threeList = filter ((== 3) . length) threeOrFourList
fourList = filter ((== 4) . length) threeOrFourList

fiveList :: [[Int]]
fiveList = 
    let potentials = [mergeLists as bs | as <- fourList, bs <- fourList]
        rightLength = filter ((== 5) . length) potentials
    in filter validList rightLength

main = do
    let answer = minimum $ map sum fiveList
    putStrLn . show $ answer