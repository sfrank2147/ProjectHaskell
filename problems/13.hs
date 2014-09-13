import System.IO

main = do
    handle <- openFile "13.txt" ReadMode
    contents <- hGetContents handle
    let numStrings = words contents
        solution = sum [read x :: Integer | x <- numStrings]
    putStrLn $ take 10 $ show solution
    hClose handle