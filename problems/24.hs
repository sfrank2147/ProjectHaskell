import Data.List

sortedPerms :: [Int] -> [[Int]]
--return a sorted list of the permutations of the argument
--assume the argument is sorted
sortedPerms [] = [[]]
sortedPerms xs = sort . permutations $ xs

main = putStrLn . show $ (sortedPerms [0,1,2,3,4,5,6,7,8,9]) !! 999999