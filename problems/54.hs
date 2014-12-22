--design a data type for Suit (enum of chars, 'C'|'D'|'H'|'S')
--design a data type for a card (a tuple of (Int, Suit)).
    --we'll let jacks be 11, queens be 12, kings be 13, aces be 14
    --write a function to convert the chars

--a hand is a tuple of cards, i.e. (Card, Card, Card, Card, Card)
    --functions to determine straight flush (really the same as royal flush) (just a straight and a flush),
    --four of a kind, full house, flush, straight,
    --three of a kind, two pairs, one pair

--design a data type for a hand
--design a comparator between hands
    --first compare type of hand
    --if type of hand is the same, compare secondary characteristics
        --for straights and flushes, highest card
            --royal flush is just a straight flush w/ highest card ace
        --for pairs and double pairs, highest pair
        --for full houses, highest triple, then highest double

import qualified Data.List as List
import qualified Data.Map.Lazy as Map

data Suit = H | S | D | C deriving (Eq, Show)

data Card = Card { suit :: Suit,
                   value :: Int
                } deriving (Show)
data Hand = Hand Card Card Card Card Card

hasFlush :: Hand -> Bool
hasFlush (Hand a b c d e) = all (== suit a) [suit b, suit c, suit d, suit e]

hasStraight :: Hand -> Bool
hasStraight (Hand a b c d e) =
    let vals = map value [a, b, c, d, e]
        sortedVals = List.sort vals
    in all (\x -> sortedVals !! x == sortedVals !! (x + 1) - 1) [0..3]

hasStraightFlush :: Hand -> Bool
hasStraightFlush hand = hasStraight hand && hasFlush hand

mostOfAKind :: Hand -> Int
mostOfAKind (Hand a b c d e) =
    let vals = map value [a, b, c, d, e]
    in maximum $ map (\val -> length $ filter (== val) vals) vals

hasFourOfAKind = (>= 4) . mostOfAKind
hasThreeOfAKind = (>= 3) . mostOfAKind
hasOnePair = (>= 2) . mostOfAKind

hasFullHouse :: Hand -> Bool
hasFullHouse (Hand a b c d e) = 
    let vals = map value [a, b, c, d, e]
        numMatching = map (\val -> length $ filter (== val) vals) vals
    in length (filter (== 3) numMatching) == 3 && length (filter (== 2) numMatching) == 2

hasTwoPair :: Hand -> Bool
hasTwoPair (Hand a b c d e) = 
    let vals = map value [a, b, c, d, e]
        numMatching = map (\val -> length $ filter (== val) vals) vals
    in length (filter (== 2) numMatching) == 4

handVals :: Hand -> [Int]
handVals (Hand a b c d e) = map value [a, b, c, d, e]

primaryPointVal :: Hand -> Int
--give higher point value to better types of hands
primaryPointVal hand
    | hasStraightFlush hand = 9
    | hasFourOfAKind hand = 8
    | hasFullHouse hand = 7
    | hasFlush hand = 6
    | hasStraight hand = 5
    | hasThreeOfAKind hand = 4
    | hasTwoPair hand = 3
    | hasOnePair hand = 2
    | otherwise = 1

--utility for counting how many times a value appears in a list
listCount :: (Eq a) => a -> [a] -> Int
listCount x xList = length $ filter (== x) xList

compareValues :: [Int] -> [Int] -> Bool
--return true if list a is "better" than list b
--compare in descending order, until one list has a higher number than another
compareValues [] [] = False
compareValues x y =
    let sortedX = List.sortBy (flip compare) x
        sortedY = List.sortBy (flip compare) y
    in if head sortedX /= head sortedY then head sortedX > head sortedY else compareValues (tail sortedX) (tail sortedY)

compareStraightFlushes :: Hand -> Hand -> Bool
compareStraightFlushes handA handB = compareValues (handVals handA) (handVals handB)

compareFlushes :: Hand -> Hand -> Bool
compareFlushes handA handB = compareValues (handVals handA) (handVals handB)

compareStraights :: Hand -> Hand -> Bool
compareStraights handA handB = compareValues (handVals handA) (handVals handB)

compareHighCards :: Hand -> Hand -> Bool
compareHighCards handA handB = compareValues (handVals handA) (handVals handB)

compareFourOfAKinds :: Hand -> Hand -> Bool
compareFourOfAKinds handA handB =
    let aVals = handVals handA
        bVals = handVals handB
        aFourVal = head $ filter (\x -> listCount x aVals == 4) aVals
        bFourVal = head $ filter (\x -> listCount x bVals == 4) bVals
    in if aFourVal > bFourVal then True else if bFourVal > aFourVal then False else compareValues aVals bVals

compareFullHouses :: Hand -> Hand -> Bool
compareFullHouses handA handB =
    let aVals = handVals handA
        bVals = handVals handB
        aThreeValList = filter (\x -> listCount x aVals == 3) aVals
        bThreeValList = filter (\x -> listCount x bVals == 3) bVals
        aTwoValList = filter (\x -> listCount x aVals == 2) aVals
        bTwoValList = filter (\x -> listCount x bVals == 2) bVals
    in if compareValues aThreeValList bThreeValList then True else if compareValues bThreeValList aThreeValList then False else compareValues aTwoValList bTwoValList

compareThreeOfAKinds :: Hand -> Hand -> Bool
compareThreeOfAKinds handA handB =
    let aVals = handVals handA
        bVals = handVals handB
        aThreeVal = head $ filter (\x -> listCount x aVals == 3) aVals
        bThreeVal = head $ filter (\x -> listCount x bVals == 3) bVals
    in if aThreeVal > bThreeVal then True else if bThreeVal > aThreeVal then False else compareValues aVals bVals

compareTwoPairs :: Hand -> Hand -> Bool
compareTwoPairs handA handB =
    let aVals = handVals handA
        bVals = handVals handB
        aTwoValList = filter (\x -> listCount x aVals == 2) aVals
        bTwoValList = filter (\x -> listCount x bVals == 2) bVals
    in if compareValues aTwoValList bTwoValList then True else if compareValues bTwoValList aTwoValList then False else compareValues aVals bVals

compareOnePairs :: Hand -> Hand -> Bool
compareOnePairs handA handB =
    let aVals = handVals handA
        bVals = handVals handB
        aTwoVal = head $ filter (\x -> listCount x aVals == 2) aVals
        bTwoVal = head $ filter (\x -> listCount x bVals == 2) bVals
    in if aTwoVal > bTwoVal then True else if bTwoVal > aTwoVal then False else compareValues aVals bVals

compareHands :: Hand -> Hand -> Bool
--return true if a is a better hand than b
compareHands a b
    | primaryPointVal a > primaryPointVal b = True
    | primaryPointVal b > primaryPointVal a = False
    --if we get to this level, it's the same type of hand
    | primaryPointVal a == 9 = compareStraightFlushes a b
    | primaryPointVal a == 8 = compareFourOfAKinds a b
    | primaryPointVal a == 7 = compareFullHouses a b
    | primaryPointVal a == 6 = compareFlushes a b
    | primaryPointVal a == 5 = compareStraights a b
    | primaryPointVal a == 4 = compareThreeOfAKinds a b
    | primaryPointVal a == 3 = compareTwoPairs a b
    | primaryPointVal a == 2 = compareOnePairs a b
    | primaryPointVal a == 1 = compareHighCards a b

--functions for getting the cards from the strings
charToCardVal :: Char -> Int
charToCardVal c =
    let lookupMap = Map.fromList [('2', 2), ('3', 3), ('4', 4), ('5', 5), ('6', 6), ('7', 7), ('8', 8), ('9', 9), ('T', 10), ('J', 11), ('Q', 12), ('K', 13), ('A', 14)]
    in lookupMap Map.! c

charToSuit :: Char -> Suit
charToSuit c = 
    let lookupMap = Map.fromList [('H', H), ('C', C), ('D', D), ('S', S)]
    in lookupMap Map.! c

cardFromString :: [Char] -> Card
--derive a card from a two-letter string
cardFromString str =
    let cardVal = charToCardVal $ str !! 0
        cardSuit = charToSuit $ str !! 1
    in Card {suit = cardSuit, value = cardVal}

handFromList :: [[Char]] -> Hand
--return a hand from a list of 5 strings
handFromList strList =
    let cards = map cardFromString strList
    in Hand (cards !! 0) (cards !! 1) (cards !! 2) (cards !! 3) (cards !! 4)

twoHandsFromLine :: [Char] -> (Hand, Hand)
twoHandsFromLine line =
    let charStrs = words line
        firstHandStrs = take 5 charStrs
        secondHandStrs = drop 5 . take 10 $ charStrs
    in (handFromList firstHandStrs, handFromList secondHandStrs)

evaluateLine :: [Char] -> Bool
--return true if the first player wins the line
evaluateLine str =
    let (a, b) = twoHandsFromLine str
    in compareHands a b

scoreText :: [Char] -> Int
scoreText text =
    let textLines = lines text
        winningLines = filter evaluateLine textLines
    in length winningLines

main = do
    content <- readFile "p054_poker.txt"
    putStrLn . show $ scoreText content
