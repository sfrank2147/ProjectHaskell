import Data.List.Split

bestPaths :: [Int] -> [Int] -> [Int]
--produce an array of ints representing the best path in a row
bestPaths prevBest curr = map (\idx -> bestPathValue prevBest idx + curr !! idx) [0..(length curr - 1)]

bestPathValue :: [Int] -> Int -> Int
bestPathValue prevBest idx
    | idx == 0                  = prevBest !! 0
    | idx == length prevBest    = prevBest !! (idx - 1)
    | otherwise                 = max (prevBest !! (idx - 1)) (prevBest !! idx)

solveTriangle :: [[Int]] -> Int
solveTriangle = maximum . foldl1 bestPaths

triFromString :: String -> [[Int]]
triFromString str =
    let rows = lines str
    in map (map read . (splitOn " ")) rows

main = do
    content <- readFile "p067_triangle.txt"
    let triangle = triFromString content
    putStrLn . show $ solveTriangle triangle