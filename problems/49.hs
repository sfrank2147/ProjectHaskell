import Prime

--first one needs to be a four digit number
--arithmetic increase b/t terms
--start w/ list of 4-digit numbers, filter by prime
--for each four-digit number, arithmetic increase must be <= (10000-prime number)

validSequences :: [(Int, Int)]
validSequences =
    let possibleStarts = [1000..9999]
        primeStarts = filter isPrime possibleStarts
        possiblePairs = [(p, diff) | p <- primeStarts, diff <- [1..((10000 - p) `div` 2)]]
        validPairs = filter isValid possiblePairs
    in validPairs

isValid :: (Int, Int) -> Bool
isValid (p, diff) =
    let candTriple = (p, p + diff, p + diff + diff)
    in isValidTriple candTriple

isValidTriple :: (Int, Int, Int) -> Bool
isValidTriple (x, y, z) = isPerm x y && isPerm x z && isPrime x && isPrime y && isPrime z

isPerm :: Int -> Int -> Bool
isPerm x y =
    let strX = show x
        strY = show y
    --this isn't actually true permutations, but it'll be quicker,
    --and I can get the right answer afterwards
    in (all (`elem` strY) strX) && (all (`elem` strX) strY)

main = putStrLn $ show validSequences