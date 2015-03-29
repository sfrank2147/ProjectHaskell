import qualified Data.Map as Map

-- This is basically the same as 76, except you're allowed to include the number itself
-- We can use Euler's partition function: http://mathworld.wolfram.com/PartitionFunctionP.html
memoizedPartitionMap :: Int -> Int -> (Map.Map Int Integer) -> (Map.Map Int Integer)
memoizedPartitionMap target n m =
    let r = if n < 0 then 0 else if n == 0 then 1 else
            -- Note that if k > sqrt(n), then both these expressions will be < 0.
             sum $ map (\k -> 
                ((-1)^(k+1)) * 
                    ((if (n - ((k * (3 * k - 1)) `div` 2)) < 0 then 0 else (m Map.! (n - ((k * (3 * k - 1)) `div` 2)))) +
                     (if (n - ((k * (3 * k + 1)) `div` 2)) < 0 then 0 else (m Map.! (n - ((k * (3 * k + 1)) `div` 2)))))
                ) [1..(floor . sqrt . fromIntegral $ n)]
        nextMap = (Map.insert n r m)
    in if n == target then nextMap else memoizedPartitionMap target (n + 1) nextMap

--answer = head $ filter (\x -> memoizedPartFn x `mod` 100 == 0) [1..]

partition :: Int -> Integer
partition n = (memoizedPartitionMap n 0 Map.empty) Map.! n

main = do
    --putStrLn . show $ partition 5
    let m = memoizedPartitionMap 100000 0 Map.empty
        answer  = head $ filter (\x -> (m Map.! x) `mod` 1000000 == 0) [1..]
    putStrLn . show $ answer