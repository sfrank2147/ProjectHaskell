import Data.Char
import System.IO
import Data.String.Utils

t_n :: Int -> Int
--return the nth triangle number
t_n n = n * (n + 1) `div` 2

letter_val :: Char -> Int
--return the letter value for the char
--for example, letter_val S is 19
letter_val c = (ord c) - 64

word_val :: String -> Int
--return the word val of the string
word_val str = sum $ map letter_val str

isPerfectSquare :: Int -> Bool
isPerfectSquare x =
    let approximateSquareRoot = maximum $ takeWhile (\y -> y*y <= x) [1..]
    in (approximateSquareRoot * approximateSquareRoot == x)

isTriangleNum :: Int -> Bool
--returns true if x is a triangle number
--consider n*(n+1)/2 is x
--solve with quadratic formula
--n is -1 +/- sqrt(1 + 8x)/2
--so x is a triangle num if x is greater than 0
--and if 8x+1 is a perfect square
isTriangleNum x = (x > 0) && (isPerfectSquare $ 8*x + 1)

isTriangleWord :: String -> Bool
--return true if the word's value is a triangle number
isTriangleWord = isTriangleNum . word_val

--countLoop :: Handle -> Int -> Int
--countLoop inputHandle curr =
--    do ineof <- hIsEOF inputHandle
--        if ineof
--            then curr
--                else do inpStr <- hGetLine inputHandle
--                    let next = if isTriangleWord inpStr then curr + 1 else curr
--                    return countLoop inputHandle next

--main = forever $ do
--    inputHandle = openFile "p042_words.txt" ReadMode
--    let result = countLoop inputHandle 0
--    putStrLn $ show result
--    hClose inputHandle

main = do
    fileText <- readFile "p042_words.txt"
    let wordListRaw = split "," fileText
        wordList = map (filter (/= '"')) wordListRaw
        triangleWordList = filter isTriangleWord wordList
    putStrLn $ show $ length triangleWordList
