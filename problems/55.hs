reverseNum :: Integer -> Integer
reverseNum = read . reverse . show

isPalindrome :: Integer -> Bool
isPalindrome num =
    let numStr = show num
    in reverse numStr == numStr

isLycrell :: Integer -> Bool
--call it recursively
isLycrell num = isLycrellRecursive (reverseNum num + num) 1

isLycrellRecursive :: Integer -> Int -> Bool
isLycrellRecursive num iter
    | iter >= 50 = True
    | isPalindrome num = False
    | otherwise = isLycrellRecursive (reverseNum num + num) (iter + 1)

main = do
    let answer = length $ filter isLycrell [1..9999]
    putStrLn . show $ answer