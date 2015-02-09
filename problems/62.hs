--basic strategy for this problem:
--keep a map:
    --key: a permutation equivalence set (indexed by that permutation's chars in alphabetical order)
    --value: a list of integers

import qualified Data.Map.Lazy as Map
import Data.List

solution :: Integer
solution = solutionRecursive Map.empty 1

solutionRecursive :: Map.Map [Char] [Integer] -> Integer -> Integer
solutionRecursive storedCubes x =
    let xCubed = x ^ 3
        xCubedPerm = sort $ show xCubed
        perms = Map.findWithDefault [] xCubedPerm storedCubes
        in if length perms == 4 
            then head perms 
            else solutionRecursive (Map.insert xCubedPerm (perms ++ [xCubed]) storedCubes) (x + 1)

main = putStrLn . show $ solution