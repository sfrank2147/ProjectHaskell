--from experimenting in ghci
--takeWhile (\x -> length(show(9^x)) == x) [1..]
--[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21]
--it has to have 21 digits or less
--otherwise it's too big (9^22 is 21, and 10^x will always be (x + 1) digits)

solution = length $ filter valid [(a, b) | a <- [1..9], b <- [1..21]]
            where valid (a, b) = length (show (a ^ b)) == b

main = putStrLn . show $ solution