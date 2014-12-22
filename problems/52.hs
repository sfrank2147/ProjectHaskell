import qualified Data.Map.Lazy as Map

containsSameDigits :: Int -> Int -> Bool
--containsSameDigits x y returns whether x and y have the same number of digits
containsSameDigits x y =
    let xStr = show x
        yStr = show y
        xDigitMap = foldl (\acc dig -> Map.insertWith (+) dig 1 acc) Map.empty xStr
        yDigitMap = foldl (\acc dig -> Map.insertWith (+) dig 1 acc) Map.empty yStr
    in Map.isSubmapOf xDigitMap yDigitMap && Map.isSubmapOf yDigitMap xDigitMap

isValidSolution :: Int -> Bool
isValidSolution x =
    let multiples = map (* x) [2..6]
    in all (containsSameDigits x) multiples

solution = head $ filter isValidSolution [1..]

main = putStrLn $ show solution