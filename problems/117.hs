-- How many ways can we lay down a combination of tiles of different lengths
-- Again, use a memoized strategy to solve it
-- Tiles can be of length 1, 2, 3, or 4
import qualified Data.Map as Map

memoTiles :: Int -> Int
-- Base cases are hardcoded - that way we can start w/ curr at 5
memoTiles 1 = 1
memoTiles 2 = 2
memoTiles 3 = 4
memoTiles 4 = 8
memoTiles x = 
    let m = Map.fromList [(1, 1), (2, 2), (3, 4), (4, 8)]
    in memoTilesRecursive x 5 m

memoTilesRecursive :: Int -> Int -> Map.Map Int Int -> Int
memoTilesRecursive x curr m = 
    let cNum =
                Map.findWithDefault 0 (curr - 1) m -- if first tile is empty
                + Map.findWithDefault 0 (curr - 2) m -- if starts w/ 2 tile
                + Map.findWithDefault 0 (curr - 3) m -- if starts w/ 3 tile
                + Map.findWithDefault 0 (curr - 4) m -- if starts w/ 4 tile
    in if curr == x then cNum else memoTilesRecursive x (curr + 1) (Map.insert curr cNum m)

main = putStrLn . show $ memoTiles 50