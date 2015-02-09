import qualified Data.Map as Map
import Data.List.Extras.Argmax

factorsMap :: Int -> (Map.Map Int Int)
--return a map mapping each factor to its multiplicity
factorsMap = factorsMapRecursive (Map.empty)

factorsMapRecursive :: (Map.Map Int Int) -> Int -> (Map.Map Int Int)
factorsMapRecursive currMap 1 = currMap
factorsMapRecursive currMap num =
    let smallestFactor = head $ filter (\x -> num `mod` x == 0) [2..]
        multiplicity = (Map.findWithDefault 0 smallestFactor currMap) + 1
    in factorsMapRecursive (Map.insert smallestFactor multiplicity currMap) (num `div` smallestFactor)

phiTerm :: Int -> Int -> Int
--what value does a given prime factor contribute to phi(n)?
phiTerm factor 1 = (factor - 1)
phiTerm factor n = (factor - 1) * (factor ^ (n - 1))

phi :: Int -> Int
phi n =
    let fm = factorsMap n
        phiTermMap = Map.mapWithKey phiTerm fm
    in  Map.fold (*) 1 phiTermMap

phiScore :: Int -> Double
phiScore n = (fromIntegral n) / (fromIntegral $ phi n)

main = do
    let answer = argmax phiScore [2..100000]
    putStrLn . show $ answer
