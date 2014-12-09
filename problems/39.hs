numRightTriangles :: Int -> Int
numRightTriangles perimeter =
    let abs = [(a,b) | a <- [1..perimeter], b <- [a..perimeter]]
        triples = filter (\(a,b) -> a^2 + b^2 == (perimeter - a - b)^2) abs
    in length triples

main = do
    let mostRightTriangles = maximum [(numRightTriangles p, p) | p <- [1..1000]]
    putStrLn $ show (fst mostRightTriangles)
    putStrLn $ show (snd mostRightTriangles)