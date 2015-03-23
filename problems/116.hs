import qualified Data.Map as Map

--numTwoCovers :: Int -> Int
---- Given a number of black square tiles, how many possible ways
---- Can you fill in the tiles using at least one two-square red tile?
--numTwoCovers x
--    | x < 2            = 0 -- You can't cover it at all
--    -- There are x starting places, but we need it to start with at least
--    -- 2 tiles left.  There's not enough room for more than 1 tile
--    | x <= (2 * 2 - 1) = x - (2 - 1)
--    -- Solve recursively - the first two spots can be covered by a red tile,
--    -- or not.
--    -- But, we add a solution, b/c if the first two spots are covered by
--    -- a red tile, we have the option of leaving the rest empty
--    -- Third option - the first tile can be empty, and the second tile can be
--    -- covered, taking up the first 3 spots.
--    | otherwise        = numTwoCovers (x - 2) 
--                         + (numTwoCovers (x - 2) + 1)
--                         + (numTwoCovers (x - 3) + 1)

--numThreeCovers :: Int -> Int
--numThreeCovers x
--    | x < 3            = 0
--    | x <= (2 * 3 - 1) = x - (3 - 1)
--    | otherwise        = numThreeCovers (x - 3)
--                         + numThreeCovers (x - 3) + 1
--                         + numThreeCovers (x - 4) + 1
--                         + numThreeCovers (x - 5) + 1

numNCovers :: Int -> Int -> Int
-- Factory for making functions
numNCovers n x
    | x < n            = 0
    | x <= (2 * n) - 1 = x - (n - 1)
    | otherwise        = numNCovers n (x - n)
                        + sum [numNCovers n (x - (n + y)) + 1 | y <- [0..(n - 1)]]

numNCoversMemoized :: Int -> Int -> Int
numNCoversMemoized n x = numNCoversMemoizedRecursive n x 1 Map.empty

numNCoversMemoizedRecursive :: Int -> Int -> Int -> Map.Map Int Int -> Int
numNCoversMemoizedRecursive n x curr m =
    let cNum =
            if curr < n then 0 else
            if curr <= (2 * n) - 1 then curr - (n - 1) else
            (Map.findWithDefault 0 (curr - n) m) + 
                sum [(Map.findWithDefault 0 (curr - (n + y)) m) + 1 | y <- [0..(n - 1)]]
    in if curr == x then cNum
        else numNCoversMemoizedRecursive n x (curr + 1) (Map.insert curr cNum m)

numTwoCovers   = numNCoversMemoized 2
numThreeCovers = numNCoversMemoized 3
numFourCovers  = numNCoversMemoized 4


main = putStrLn . show $ (numTwoCovers 50) + (numThreeCovers 50) + (numFourCovers 50)