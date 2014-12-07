import Data.List
import Data.Maybe

memoizedFibonacci :: [Integer]
memoizedFibonacci = map fib [0..]
    where fib 0 = 1
          fib 1 = 1
          fib x = (memoizedFibonacci !! (x-1)) + (memoizedFibonacci !! (x-2))

main = do
    let val = head (filter (\x -> length (show x) == 1000) memoizedFibonacci)
    putStrLn . show $ fromJust (elemIndex val memoizedFibonacci) + 1
