integerPowerMod :: Integer -> Integer -> Integer -> Integer
integerPowerMod x 1 m = x `mod` m
integerPowerMod x 0 _ = 1
integerPowerMod x y m = (x * (integerPowerMod x (y-1) m)) `mod` m

sumMod :: [Integer] -> Integer -> Integer
sumMod xs m = foldl (\acc x -> (acc + x) `mod` m) 0 xs

main = do
    let powers = map (\x -> integerPowerMod x x 10000000000) [1..1000]
        answer = sumMod powers 10000000000
    putStrLn $ show answer