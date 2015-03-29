-- We want a fraction that's extremely close to 3/7, but not equivalent.
-- First, get a list of all denominators not divisible by 7.
-- For each one, find floor(3/7 * d).  Store this as a pair (num, denom)
-- Then choose the one that minimizes (3/7 - n/d)
import Data.List.Extras.Argmax

answer :: (Int, Int)
answer =
    let possibleDenoms = filter (\d -> d `mod` 7 /= 0) [8..1000000]
        candidatePairs = [((floor ((fromIntegral d) * 3.0 / 7.0)), d) | d <- possibleDenoms]
    in argmin (\(n, d) -> (fromIntegral 3) / 7.0 - (fromIntegral n) / (fromIntegral d)) candidatePairs

main = putStrLn . show $ answer