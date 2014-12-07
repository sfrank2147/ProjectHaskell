main = do
    let pandigitalProducts = filter isPandigitalProduct [1..9999]
        pandigitalSum = sum pandigitalProducts
    putStrLn $ show pandigitalSum

--returns True if prod can be written as a pandigital product
isPandigitalProduct :: Int -> Bool
isPandigitalProduct prod =
    let divs = divisors prod
        prodPairs = [(x, prod `div` x) | x <- divs]
        pandigitalSets = map (\(x,y) -> isPandigitalSet x y prod) prodPairs
    in any (==True) pandigitalSets

--returns true if the numbers 1 - 9 are each represented exactly once
isPandigitalSet :: Int -> Int -> Int -> Bool
isPandigitalSet x y z =
    let xString = show x
        yString = show y
        zString = show z
        combinedString = xString ++ yString ++ zString
    in sameString combinedString "123456789"

sameString :: String -> String -> Bool
sameString string1 string2 = (length string1 == length string2) && contains string1 string2 && contains string2 string1

--contains(x,y) returns True if x contains every element of y
contains :: String -> String -> Bool
contains x [] = True
contains x (y:ys) = (elem y x) && contains x ys

--return the divisors of x
divisors :: Int -> [Int]
divisors x = filter (\y -> x `mod` y == 0) [1..x]