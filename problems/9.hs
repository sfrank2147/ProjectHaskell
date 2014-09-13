validTriple :: (Int, Int, Int) -> Bool
validTriple (a, b, c) = (a^2 + b^2 == c^2)

sumToThousand :: [(Int, Int, Int)]
sumToThousand =
    let aVals = [1..333]
        bVals = [2..666]
        combinedVals = [(a,b) | a <- aVals, b <- bVals]
        validAB = filter (\(a,b) -> a < b) combinedVals
        triples = [(a,b,1000-a-b) | (a,b) <- validAB]
    in filter (\(a,b,c) -> a < c && b < c) triples

specialTriplet :: (Int, Int, Int)
specialTriplet = head $ filter validTriple sumToThousand
    --let sumToThousand = [(a, b, (1000 - a - b)) | a <- [1..333], b <- [2..666], a < b]

main = putStrLn $ show specialTriplet