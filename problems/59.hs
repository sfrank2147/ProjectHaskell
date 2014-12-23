--idea
--for each possible decryption key: (26^3)
    --decrypt the text
    --check how many instances of " the " there are
--return the key with the most instances

--to decrypt:
--message[x] = original_message[x] ^ (decryption_key[x `mod` e])
--message = map (\x -> original_message !! x ^ decryption_key !! (x `mod` 3)) [0..(length original_message - 1)]

import Data.Char
import Data.Bits
import Data.List.Split
import Data.List.Extras.Argmax

xorChars :: Char -> Char -> Char
--xor two chars based on ascii values
xorChars a b = chr $ (ord a) `xor` (ord b)


decrypt :: String -> [Char] -> String
--decrypt the string (the first argument)
--the decryption key is a list of three lower-case characters
decrypt msg key
    | length key /= 3 = error "Encryption key is not 3 letters long"
    | otherwise = zipWith xorChars msg (cycle key)

messageScore :: String -> Int
--give the score for a message
--at first, based on how many times " the " appears
messageScore msg = (length . splitOn " the " $ msg) - 1

bestKey :: String -> [Char]
bestKey msg = 
    let possibleKeys = [[a, b, c] | a <- ['a'..'z'], b <- ['a'..'z'], c <- ['a'..'z']]
    in argmax (messageScore . decrypt msg) possibleKeys

main = do
    rawContents <- readFile "p059_cipher.txt"
    let numCharList = splitOn "," rawContents
    let encryptedMessage = map (chr . read) numCharList
    let key = bestKey encryptedMessage
    putStrLn . show $ key
    let originalMessage = decrypt encryptedMessage key
    let answer = sum $ map ord originalMessage
    putStrLn $ "Final answer: " ++ (show answer)
    putStrLn originalMessage