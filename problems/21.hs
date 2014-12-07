sumOfDivisors :: Int -> Int
sumOfDivisors x = sum [y | y <- [1..(x-1)], x `mod` y == 0]

amicable :: Int -> Bool
amicable x = 
    let partner = sumOfDivisors x
        partnersPartner = sumOfDivisors partner
    in partnersPartner == x && partner /= x

main = putStrLn . show $ sum (filter amicable [2..10000])
