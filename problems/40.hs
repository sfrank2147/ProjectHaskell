--1-9: 1-9
--10-99: 10 - 189 (add 180)
--100 - 999: 190 - 2889 (add 2700)
--1000 - 9999: 2890 - 38889(add 36000)
--10000 - 99999: 38890 - 488889 (add 450000)
--100000-999999: 488890 - 5888889 (add 5400000)

--d_1: 1
--d_10: 1
--d_100: 91st digit, starting with 10. 1st digit of 46th number. 5
--d_1000: 

main = do
    let longStr = foldl (\acc x -> acc ++ (show x)) "" [0..300000]
    let solution = (digitToInt longStr!!1) * (digitToInt longStr!!10) * (digitToInt longStr!!100) * (digitToInt longStr!!1000) * (digitToInt longStr!!10000) * (digitToInt longStr!!100000) * (digitToInt longStr!!1000000)
    putStrLn $ show solution
