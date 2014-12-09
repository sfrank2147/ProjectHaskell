pandigital :: Int -> Bool
pandigital num =
    let numString = show num
    in length numString == 9 && all ((flip elem) numString) ['1','2','3','4','5','6','7','8','9']

pandigitalProducts :: [Int]
pandigitalProducts =
    let baseNums = [9000..9999]
        concatNums = map (\x -> read ((show x) ++ (show (x*2)))) baseNums
    in filter pandigital concatNums

main = putStrLn $ show pandigitalProducts