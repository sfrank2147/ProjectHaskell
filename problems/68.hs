-- Note that it takes .214 seconds to generate all permutations of length 10
-- Represent a 5-gon in terms of 10 fields: outer1, outer2, ..., outer5, inner1, ..., inner5.
-- outer1 should be the smallest of the outers - if not, our constructor will reject it.
-- Then to see if a 5-gon is valid, we just need to see if all the sums add up correctly.

-- This program runs in 1.83 seconds.

import Data.List
import Data.Maybe

data Fivegon = Fivegon { outer1 :: Int
                       , outer2 :: Int
                       , outer3 :: Int
                       , outer4 :: Int
                       , outer5 :: Int
                       , inner1 :: Int
                       , inner2 :: Int
                       , inner3 :: Int
                       , inner4 :: Int
                       , inner5 :: Int
                       } deriving Show

fiveGonFromList :: [Int] -> Maybe Fivegon
fiveGonFromList p =
    if length p /= 10 then Nothing else
    if (p !! 0) /= (minimum (take 5 p)) then Nothing else
    Just Fivegon {
        outer1 = p !! 0,
        outer2 = p !! 1,
        outer3 = p !! 2,
        outer4 = p !! 3,
        outer5 = p !! 4,
        inner1 = p !! 5,
        inner2 = p !! 6,
        inner3 = p !! 7,
        inner4 = p !! 8,
        inner5 = p !! 9
    }

isMagic :: Fivegon -> Bool
isMagic f =
    let firstSum  = (outer1 f) + (inner1 f) + (inner2 f)
        secondSum = (outer2 f) + (inner2 f) + (inner3 f)
        thirdSum  = (outer3 f) + (inner3 f) + (inner4 f)
        fourthSum = (outer4 f) + (inner4 f) + (inner5 f)
        fifthSum  = (outer5 f) + (inner5 f) + (inner1 f)
    in all ((==) firstSum) [secondSum, thirdSum, fourthSum, fifthSum]

digitString :: Fivegon -> String
-- Returns the digit string of the Fivegon
digitString f = foldl (++) "" . map show $
                         [outer1 f,
                          inner1 f,
                          inner2 f,
                          outer2 f,
                          inner2 f,
                          inner3 f,
                          outer3 f,
                          inner3 f,
                          inner4 f,
                          outer4 f,
                          inner4 f,
                          inner5 f,
                          outer5 f,
                          inner5 f,
                          inner1 f
                         ]

answer :: Integer
answer =
    let allPerms = permutations [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
        allFiveGons = map fromJust . filter isJust $ map fiveGonFromList allPerms
        magicFiveGons = filter isMagic allFiveGons
        sixteenDigitFiveGonStrings = filter ((==) 16 . length) (map digitString magicFiveGons)
        digitStringsAsIntegers = map read sixteenDigitFiveGonStrings
    in maximum digitStringsAsIntegers

main = do
    putStrLn . show $ answer