factorial :: Integer -> Integer
factorial 0 = 1
factorial x = x * factorial (x - 1)

choose :: Integer -> Integer -> Integer
--arbitrary precision integers
--calculate n choose r
choose n r = (factorial n) `div` (factorial r * (factorial (n - r)))

main = do
    let combos = [(n,r) | n <- [1..100], r <- [0..n]]
        validCombos = filter (\(n, r) -> n `choose` r > 1000000) combos
    putStrLn $ show $ length validCombos