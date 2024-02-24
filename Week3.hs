-- We don't import '||' from the prelude, so that we can 
-- define our own version

import Prelude hiding ((||), (&&), gcd) 

-- The following line declares the || operator (which we are about to
-- re-define) to be right associative and to have precedence 2. This
-- essary in oris necder for expressions such as False || x > 2 to be
-- valid (e.g. it sets the precedence of || to be lower than >). 

infixr 2  ||
infixr 3  &&

-- A naive re-implementation of the Prelude operator ||
(||) :: Bool -> Bool -> Bool
True || True    = True
False || True   = True
True || False   = True
False || False  = False

-- An alternative re-implementation
--(||) :: Bool -> Bool -> Bool
--False || False   = False
--_ || _           = True

-- Another alternative re-implementation
--(||) :: Bool -> Bool -> Bool
--True || _     =  True
--False || a    = a


fact :: Int -> Int 
fact n 
    | n == 0    = 1
    | n > 0     = n * fact (n - 1)
    | otherwise = error "factorials not defined for negative ints"

mult :: Int -> Int -> Int
mult n m 
    | n == 0        = 0
    | n > 0         = m + mult (n - 1) m 

divide :: Int -> Int -> Int
divide n m
    | n < m         = 0
    | otherwise     = 1 + divide (n - m) m

nor :: Bool -> Bool -> Bool
nor False x = not x
nor True _ = False

fibonacci :: Int -> Int
--fibonacci n
--    | n == 0 = 0
--    | n == 1 = 1
--    | otherwise  = fibonacci (n - 1) + fibonacci (n - 2)
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)


-- (&&) :: Bool -> Bool -> Bool
-- True && True    = True
-- False && True   = False
-- True && False   = False
-- False && False  = False

-- (&&) :: Bool -> Bool -> Bool
-- True && True   = True
-- _ && _           = False

(&&) :: Bool -> Bool -> Bool
False && _     =  False
True && a    = a

exOr :: Bool -> Bool -> Bool
exOr False a = a
exOr True a = not a

ifThenElse :: Bool -> Int -> Int -> Int
ifThenElse True b _ = b
ifThenElse False _ c = c 


daysInMonth :: Int -> Int
daysInMonth 2 = 28 
daysInMonth 4 = 30
daysInMonth 6 = 30
daysInMonth 9 = 30
daysInMonth 11 = 30
daysInMonth _ = 31

validDate :: Int -> Int -> Bool
validDate day month = day <= daysInMonth month && day >= 0

-- sumNumbers :: Int -> Int
-- sumNumbers 0 = 0
-- sumNumbers n = sumNumbers (n - 1) + n

sumNumbers :: Int -> Int
sumNumbers n
    | n == 0 = 0
    | otherwise = sumNumbers (n - 1) + n


-- sumSquares :: Int -> Int
-- sumSquares 0 = 0
-- sumSquares n = sumSquares(n - 1) + n ^ 2

sumSquares :: Int -> Int
sumSquares n
    | n == 0 = 0
    | otherwise = sumSquares (n - 1) + n ^ 2

-- power :: Int -> Int -> Int
-- power _ 0 = 1
-- power n p = power n (p - 1) * n

power :: Int -> Int -> Int
power n p
    | p == 0 = 1
    | otherwise = power n (p - 1) * n

sumFromTo :: Int -> Int -> Int
sumFromTo n m
    | n > m = 0
    | n == m = n
    | otherwise = sumFromTo n (m - 1) + m

gcd :: Int -> Int -> Int
gcd n m
    | n == m = n
    | n > m = gcd m (n - m)
    | otherwise = gcd n (m - n)

intSquareRoot :: Int -> Int
intSquareRoot n = findRoot n n

findRoot :: Int -> Int -> Int
findRoot n s
    | s * s <= n = s
    | otherwise = findRoot n (s - 1)

