import qualified Data.Set as Set
import qualified Data.Map as Map

-- Iterate over all integer pairs a, b s.t. a + b <= 1.5 million and a < b.
-- How many such pairs are there? sum_{i=1}^{1500000}(1500000 - i)
-- That's approximately 1.5 million ^2 over 2, which is in the trillions (no go)

-- What are some restrictions on a valid right triangle?
-- * We can arbitrarily set a < b
-- * We must have both a and b less than c.
-- * But, we must have c < a + b.
-- * So, say we fix L.  We can iterate over a from 1 to L/3.
-- * c is the largest, so c > L/3.
-- * a can't be more than L/3, or else the sides don't add up.
-- * b can range from a to (2/3L - a), because if b > (2/3L - a), then
--   a + b > 2/3L, so c < L/3.  No no.
-- So: a from 1 to L/3, b from a to (2/3L - a), c then fixed (see if triangle works)
-- How long is that?  If L is 1.5 million, a is 500000, b is 500000

-- Note that if c is odd, c^2 is divisible by 4, so a and b must both be even.

-- Breakthrough!  If a and L are fixed, b and c are forced.  Why?  Have two equations:
-- a + b + c = L
-- a^2 + b^2 = c^2
-- Substitute b = L - a - c, and you get:
-- a^2 + (L - a - c)^2 = c^2
-- a^2 + L^2 - L*a - L*c - a*L + a^2 + a*c - c*L + c*a + c^2 = c^2
-- 2a^2 + L^2 - 2*L*a - 2*L*c + 2*a*c = 0
-- (2*a - 2*L)c = 2*L*a - 2a^2 - L^2
-- c = (2*L*a - 2a^2 - L^2)/(2*a - 2*L)
-- Let's check: Set L = 12, a = 3.  Then must have b = 4 and c = 5.
-- c = (2 * 12 * 3 - 2*3^2 - 12^2)/(2*12 + 2*3) = (72 - 18 - 144)/(6 - 24),
-- which is (-90)/(-18), which is 5.  Correct!

-- Further, note that if a and c and L are integers, b must be one also.

-- So, the plan is as follows:
-- For each L from 15 to 1500000:
--    Check each a from 1 to L/3 (fix a to be the smallest side)
--    Calculate what c must be, check that it's an integer
--    Count that the number of valid as is exactly 1.

-- What's the computation involved in this?
-- 1.5 million values of L times at most 500,000 values of a.
-- sum_{L = 1}^{1.5 million} (L/3)
-- Which is 1/3 * sum_{L=1}^{1.5 million}L = 1/3 * 1/2 * (1.5 *10^6)^2
-- = 1/6 * 2.25 * 10^12 = 3.75 * 10^11 = 375 billion ops
-- Could be doable, let's see

-- I'm encountering another problem - you could have b be equal to 1/3 l,
-- a be less than 1/3 l, and you get the same answer twice.
-- In the check if it's a valid right triangle, should also check that
-- a < l - c

-- Problem: this strategy takes 36 seconds for l <= 15000.  Take way
-- too long for up to 1.5 million'

-------------------

-- A completely different track - is there a way I could enumerate all of the
-- pythagorean triples?

-- Need a and c s.t. c^2 - a^c is a perfect square.
-- Means that (c + a)(c - a) is a perfect square.
-- 
-- Process:
-- First, get all perfect squares, just Set.fromList [b^2 | b <- [1..1500000]]

-------
-------
-------

-- What I finally did:

-- According to wikipedia, all primitive pythagorean triples are generated
-- by the formulas: a = m^2 - n^2, b = 2mn, c = m^2 + n^2
-- for coprime integers m, n, one of which is even

-- The triangle inequality theorem tells us that a + b >= c.
-- So, c must be les than or equal to 750,000.
-- Since c is m^n + n^2, this tells us that m (the larger value)
-- cannot be more than sqrt(750,000), which is about 867

type Triple = (Int, Int, Int)

myGcd :: Int -> Int -> Int
myGcd 0 n = n
myGcd m 0 = m
myGcd m n = if m > n then myGcd n (m `mod` n) else myGcd m (n `mod` m)

areCoprime :: Int -> Int -> Bool
areCoprime m n = (myGcd m n) == 1

primitiveTriples :: [Triple]
primitiveTriples = [(m^2 - n^2, 2 * m * n, m^2 + n^2) | m <- [2..867],
                                                       n <- [1..(m - 1)],
                                                       (m - n) `mod` 2 == 1,
                                                       areCoprime m n]

tripleMultiples :: Triple -> [Triple]
tripleMultiples (a, b, c) = takeWhile (\(a', b', c') -> a' + b' + c' <= 1500000)
                                [(k*a, k*b, k*c) | k <- [1..]]

allTriples :: [Triple]
allTriples = concat $ map tripleMultiples primitiveTriples

updateMap :: (Map.Map Int Int) -> Triple -> (Map.Map Int Int)
updateMap m (a, b, c) =
    let l = a + b + c
    in case (Map.lookup l m) of
        Nothing -> Map.insert l 1 m
        Just t  -> Map.insert l (t + 1) m

main = do
    let mapCount = foldl updateMap (Map.empty) allTriples
    putStrLn . show $ Map.size (Map.filter ((==) 1) mapCount)
