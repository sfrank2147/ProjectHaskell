data TwoDigitNumber = TwoDigitNumber Int Int deriving (Show)

--get the int value of a TwoDigitNumber
intValue :: TwoDigitNumber -> Int
intValue (TwoDigitNumber a b) = 10 * a + b

data TwoDigitFraction = TwoDigitFraction TwoDigitNumber TwoDigitNumber deriving (Show)

fracNum :: TwoDigitFraction -> Int
fracNum (TwoDigitFraction x y) = intValue x

fracDenom :: TwoDigitFraction -> Int
fracDenom (TwoDigitFraction x y) = intValue y

--check if the values of two twodigitFractions are Equal
areEqual :: TwoDigitFraction -> TwoDigitFraction -> Bool
areEqual (TwoDigitFraction a b) (TwoDigitFraction c d) = 
    (intValue a) * (intValue d) == (intValue b) * (intValue c)

--returns True if the silly division rule applies
sillyFrac :: TwoDigitFraction -> Bool
sillyFrac (TwoDigitFraction (TwoDigitNumber a b) (TwoDigitNumber c d)) =
    (b == c && areEqual (TwoDigitFraction (TwoDigitNumber a b) (TwoDigitNumber c d)) (TwoDigitFraction (TwoDigitNumber a 0) (TwoDigitNumber d 0)))
    || (a == d && areEqual (TwoDigitFraction (TwoDigitNumber a b) (TwoDigitNumber c d)) (TwoDigitFraction (TwoDigitNumber b 0) (TwoDigitNumber c 0)))
    && areEqual (TwoDigitFraction (TwoDigitNumber a b) (TwoDigitNumber c d)) (TwoDigitFraction (TwoDigitNumber a 0) (TwoDigitNumber c 0))
    && (intValue (TwoDigitNumber a b) < intValue (TwoDigitNumber c d))

main = do
    let twoDigitNums = [TwoDigitNumber x y | x <- [1..9], y <- [1..9]] --don't care about nums with 0 at the end
        allFracs = [TwoDigitFraction num denom | num <- twoDigitNums, denom <- twoDigitNums]
        fracs = filter (\(TwoDigitFraction x y) -> not (intValue x == intValue y)) allFracs
        sillyFracs = filter sillyFrac fracs
        numProduct = product $ map fracNum sillyFracs
        denomProduct = product $ map fracDenom sillyFracs
    putStrLn $ show numProduct
    putStrLn $ show denomProduct

