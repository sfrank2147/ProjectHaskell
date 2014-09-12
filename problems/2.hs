main = putStrLn (show sumOfSmallEvens)

sumOfSmallEvens = sum $ filter even $ takeWhile (<4000000) fibList

fibList :: [Int]
fibList = scanl (+) 1 ([1] ++ fibList)

--fibList:    1, 2, 3, 5
--other list: 1, 1, 2, 3
