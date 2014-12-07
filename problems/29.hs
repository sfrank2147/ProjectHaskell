import qualified Data.Set as Set

main = putStrLn . show $ distinctTerms 100 100

distinctTerms :: Integer -> Integer -> Int
distinctTerms a b =
    let pairs = [(x,y) | x <- [2..a], y <- [2..b]]
        finalSet = foldl (\powerSet pair -> Set.insert ((fst pair) ^ (snd pair)) powerSet) Set.empty pairs
        in Set.size finalSet