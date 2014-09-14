import Data.List

main = do
    let collatzNums = map collatz [1..999999]
    let maxCollatzNum = maximum collatzNums
    let almostSolution = (elemIndex maxCollatzNum collatzNums)
    let solution = fmap (+1) almostSolution
    putStrLn (show solution)

collatz :: Int -> Int
collatz 1 = 1
collatz x
    | x `mod` 2 == 0    = 1 + collatz (div x 2)
    | otherwise         = 1 + collatz (3*x + 1)