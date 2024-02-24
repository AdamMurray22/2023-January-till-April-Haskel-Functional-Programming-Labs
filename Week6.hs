{- Week6.hs
 This module illustrates the use of functions as values
-}

import Data.Char

twice :: (Int -> Int) -> Int -> Int
twice f x = f (f x)

multiply :: Int -> Int -> Int
multiply x y = x * y

double :: Int -> Int
double = multiply 2

doubleAll :: [Int] -> [Int]
doubleAll = map (*2)

areDigits :: String -> [Bool]
areDigits = map isDigit

keepPositive :: [Int] -> [Int]
keepPositive = filter (>0)

keepDigits :: String -> String
keepDigits = filter isDigit

addUp :: [Int] -> Int
addUp = foldr (+) 0 

myConcat :: [[a]] -> [a]
myConcat = foldr (++) []

alwaysEven :: (Int -> Int) -> [Int] -> Bool
-- alwaysEven f xs = length (filter even (map f xs)) == length xs
-- alwaysEven f xs = andAll (map (even . f) xs)
alwaysEven f = andAll . map (even . f)

andAll :: [Bool] -> Bool
andAll xs = foldr (&&) True xs

updatePositivesOnly :: (Float -> Float) -> [Float] -> [Float]
updatePositivesOnly f xs = map (\x -> if x > 0 then f x else x) xs
-- updatePositivesOnly _ [] = []
-- updatePositivesOnly f (x : xs)
--     | x > 0 = f x : updatePositivesOnly f xs
--     | otherwise = x : updatePositivesOnly f xs

mult10 :: [Int] -> [Int]
mult10 xs = map (*10) xs

-- onlyLowerCase :: String -> String
-- onlyLowerCase s = filter isLower s

orAll :: [Bool] -> Bool
orAll xs = foldr (||) False xs

sumSquares :: [Int] -> Int
sumSquares xs = foldr (+) 0 (map (^2) xs)

zeroToTen :: [Int] -> [Int]
zeroToTen xs = filter (\x -> x <= 10 && x >= 0) xs

squareRoots :: [Float] -> [Float]
squareRoots xs = map sqrt (filter (>=0) xs)

countBetween :: Float -> Float -> [Float] -> Int
countBetween l u xs = length (filter (<=u) (filter (>=l)  xs))

-- alwaysPositive :: (Float -> Float) -> [Float] -> Bool
-- alwaysPositive f xs = length (filter (>0) (map f xs)) == length xs

-- alwaysPositive :: (Float -> Float) -> [Float] -> Bool
-- alwaysPositive f xs = andAll (map ((>0) . f) xs)

-- alwaysPositive :: (Float -> Float) -> [Float] -> Bool
-- alwaysPositive f = andAll . map ((>0) . f)

productSquareRoots :: [Float] -> Float
productSquareRoots xs = foldr (*) 1 (squareRoots xs)

removeFirst :: (a -> Bool) -> [a] -> [a]
removeFirst f (x:xs) = remove f (x:xs) True
    where 
        remove :: (a -> Bool) -> [a] -> Bool -> [a]
        remove f (x:xs) b
            | not b = xs
            | f x = xs
            | otherwise = x:remove f xs b
        remove f xs b = xs

removeLast :: (a -> Bool) -> [a] -> [a]
removeLast f (x:xs) = reverse (remove f  (reverse (x:xs)) True)
    where 
        remove :: (a -> Bool) -> [a] -> Bool -> [a]
        remove f (x:xs) b
            | not b = xs
            | f x = xs
            | otherwise = x:remove f xs b
        remove f xs b = xs