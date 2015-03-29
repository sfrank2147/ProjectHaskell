-- This problem can be reduced to asking:
-- For a fixed d, how many numerators n have the property that gcf(n, d) = 1
-- and n/d is between 1/3 and 1/2

-- That's pretty easy to calculate.
-- First, get a list of all the numbers between d/3 and d/2, non-inclusive.
-- Then, filter based on the ones that are relatively prime.
-- Finally, take the length.

-- This code runs in 5.30 seconds on my macbook.

numFractionsWithDenom :: Int -> Int
numFractionsWithDenom d =
    -- If d is divisible by 3, divide by 3, and we can add one or not
    -- (b/c we don't want d/3 exactly, but it's not relatively prime)
    -- If not, divide by 3 (rounding down) and add one anyway.
    -- So subtracting works in both cases.
    let lowerNumBound = (d `div` 3) + 1
    -- If d is divisible by 2, divide by 2, and we can subtract one or not
    -- (b/c we don't want d/2 exactly, but it's not relatively prime)
    -- If d isn't divisible by 2, we divide by 2 (rounding down) and keep that
    -- So not subtracting works in both cases.
        upperNumBound = d `div` 2
        possibleNums = [lowerNumBound..upperNumBound]
    in length $ filter ((==) 1 . gcd d) possibleNums

answer = sum $ map numFractionsWithDenom [4..12000]

main = putStrLn . show $ answer
