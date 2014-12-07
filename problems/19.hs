normalYear = [31,28,31,30,31,30,31,31,30,31,30,31]
leapYear = [31,29,31,30,31,30,31,31,30,31,30,31]
years = take (12*100) $ (cycle (normalYear ++ normalYear ++ normalYear ++ leapYear))
monthBeginnings = scanl (+) 0 years
numStartingSundays = length $ filter (\x -> x `mod` 7 == (365+6) `mod` 7) (init monthBeginnings)

main = putStrLn . show $ numStartingSundays