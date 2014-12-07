import Data.Char
import Text.Regex
import System.IO
import Data.List

letterValue :: Char -> Int
letterValue l = (ord l) - (ord 'A') + 1



--function that extracts the names once we get the contents of the file
extractNames :: String -> [String]
extractNames content =
    let commaRegex = mkRegex ","
        nameListWithQuotes = splitRegex commaRegex content
        quoteRegex = mkRegex "\""
    in  map (\name -> subRegex quoteRegex name "") nameListWithQuotes

scoreName :: String -> Int
scoreName name = sum $ fmap letterValue name

main = do
    withFile "p022_names.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        let names = extractNames contents
        let sortedNames = sort names
        let nameScores = fmap scoreName sortedNames
        let totalScore = sum $ fmap (\(a, b) -> a * b) (zip nameScores [1..(length names)])
        putStrLn . show $ totalScore
        )