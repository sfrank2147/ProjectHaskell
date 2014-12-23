data Fraction = Fraction Integer Integer deriving (Show)

simplify :: Fraction -> Fraction
simplify (Fraction num den) = let d = gcd num den in Fraction (num `div` d) (den `div` d)

add :: Fraction -> Fraction -> Fraction
add (Fraction a b) (Fraction c d) =
    let summedFrac = Fraction (a * d + b * c) (b * d)
    in simplify summedFrac

invert :: Fraction -> Fraction
invert (Fraction a b) = Fraction b a

addInt :: Integer -> Fraction -> Fraction
addInt int frac = let intFrac = Fraction int 1 in add intFrac frac

--utility adding functions
addOne = addInt 1
addTwo = addInt 2

--algorithm for continued fractions:
--take what you had previously
--add 1 to it
--invert the fraction
--add 1 to it

continuedFrac :: Int -> Fraction
continuedFrac totalIters = simplify $ continuedFracRecursive 1 totalIters (Fraction 1 1)

continuedFracRecursive curr max frac
    | curr > max = frac
    | otherwise  = continuedFracRecursive (curr + 1) max (addOne . invert . addOne $ frac)

numDigits :: Integer -> Int
numDigits = length . show

--our test for if the numerator int is larger
largerNumerator :: Fraction -> Bool
largerNumerator (Fraction num den) = numDigits num > numDigits den

--this function is inefficient b/c it recalculates a lot of continued fractions
--but it runs in under a minute, so i didn't refactor
main = do
    let answer = length $ filter largerNumerator (map continuedFrac [1..1000])
    putStrLn . show $ answer