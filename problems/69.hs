import Prime
import qualified Data.Set as Set
import Data.List.Extras.Argmax

--note: in the end, this program was useless
--simply note that any prime factor only increases n/phi(n)
--if its multiplicity is 1
--also, p/(p-1) is greater if p is smaller
--so simply multiply primes while we're leq 1000000

phi :: Int -> Int
phi = phiRecursive Set.empty 1

phiRecursive:: (Set.Set Int) -> Int -> Int -> Int
--calculate the totient function recursively
--we're passed in a set of primes we've already encountered
phiRecursive _ curr 1 = curr
phiRecursive s curr n =
    let sd = head $ filter (\d -> n `mod` d == 0) [2..n]
        multiple = if Set.member sd s then sd else (sd - 1)
    in phiRecursive (Set.insert sd s) (curr * multiple) (n `div` sd)

phiScore :: Int -> Double
phiScore x = (fromIntegral x) / (fromIntegral $ phi x)

main = do
    let answer = argmax phiScore [2..1000000]
    putStrLn . show $ answer