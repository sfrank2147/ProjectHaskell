--how many ways can I make 2 euros?  i.e. 200 pence
--i can use 200 pence, 100 pence, 50, 20, 5, 2, and 1
--i want to avoid repeats: i.e. 100, 50, 50 is the same as 50, 100, 50

--this lends itself well to recursion, except that we need to keep track of previously issued coins

numCombinations :: Int -> [Int] -> Int
--param 1: the value
--param 2: the coins we can use
--param 3: the number of combinations

--if it's only one cent, there's only one thing we can do
numCombinations _ [] = 0
numCombinations 0 _ = 1
numCombinations 1 _ = 1

--we want to go from largest coin to highest coin
--there are two options
--either use up one of the highest coins, and try again with the highest coin
--or else try with all of the lower coins
--we can't use the highest coin if it's bigger than val though
numCombinations val (c:cs)
    | c > val     = numCombinations val cs --just try the second biggest coin, same thing
    | otherwise   = (numCombinations (val - c) (c:cs)) + numCombinations val cs

main = putStrLn . show $ numCombinations 200 [200, 100, 50, 20, 10, 5, 2, 1]