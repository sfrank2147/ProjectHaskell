--how do we know if a number can be written as the sum of a prime and twice a square?
--for each n s.t. 2n^2 < x, see if difference is a prime

import Prime

satisfiesConjecture :: Int -> Bool
satisfiesConjecture x =
    --formulated as x = p + 2 * n^2
    let possibleN = takeWhile (\n -> 2*n^2 < x) [1..]
        validN = filter (\n -> isPrime (x - 2*n^2)) possibleN
    in length validN > 0

main = do
    let answer = head $ filter (\x -> x `mod` 2 == 1 && not (satisfiesConjecture x) && not (isPrime x)) [3..]
    putStrLn . show $ answer