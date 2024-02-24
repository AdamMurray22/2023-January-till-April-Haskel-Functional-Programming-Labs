{- Week5.hs
 This file illustrates list patterns and recursion over lists.
-}

import Prelude hiding (fst, snd, head, tail, sum, concat, reverse, zip)
import GHC.Base (VecElem(Int16ElemRep))

-- Definitions of the prelude functions fst and snd

fst (x,_)       = x
snd (_,y)       = y

-- Definitions of the prelude functions head and tail

head (x:_)      = x
tail (_:xs)     = xs

absFirst :: [Int] -> Int
absFirst []     = -1
absFirst (x:xs) = abs x

sum :: [Int] -> Int 
sum []     = 0
sum (x:xs) =   x + sum xs

doubleAll :: [Int] -> [Int]
doubleAll []      = []
doubleAll (x:xs)  = 2*x : doubleAll xs

concat :: [[a]] -> [a]
concat []         = []
concat (x:xs)     = x ++ concat xs

reverse :: [a] -> [a]
reverse []      = []
reverse (x:xs)  = reverse xs ++ [x]

zip :: [a] -> [b] -> [(a,b)]
zip (x:xs) (y:ys)  = (x,y) : zip xs ys
zip _ _            = []

type StudentMark = (String, Int)
testData :: [StudentMark]
testData =
    [ ("John", 53),
    ("Sam", 16),
    ("Kate", 85),
    ("Jill", 65),
    ("Bill", 37),
    ("Amy", 22),
    ("Jack", 41),
    ("Sue", 71)
    ]

countSpaces :: String -> Int
-- countSpaces "" = 0
-- countSpaces (x : xs)
-- | x == ' ' = 1 + countSpaces xs
-- | otherwise = countSpaces xs
countSpaces xs = sum [1 | x <- xs, x == ' ']

mergeLists :: [Int] -> [Int] -> [Int]
mergeLists [] [] = []
mergeLists [] ys = ys
mergeLists xs [] = xs
mergeLists (x:xs) (y:ys)
    | x <= y = x : mergeLists xs (y:ys)
    | otherwise = y : mergeLists (x:xs) ys

headPlusOne :: [Int] -> Int
headPlusOne [] = -1
headPlusOne (x:xs) = x + 1

duplicateHead :: [a] -> [a]
duplicateHead [] = []
duplicateHead (x:xs) = x:x:xs

rotate :: [a] -> [a]
rotate [] = []
rotate (x:[]) = [x]
rotate (x1:x2:xs) = x2:x1:xs

listLength :: [a] -> Int
listLength [] = 0
listLength (x:xs) = 1 + listLength xs

multAll :: [Int] -> Int
multAll [] = 1
multAll (x:xs) = x * multAll xs

andAll :: [Bool] -> Bool
andAll [] = True
andAll (x:xs) = x && andAll xs

orAll :: [Bool] -> Bool
orAll [] = False
orAll (x:xs) = x || orAll xs

countIntegers :: Int -> [Int] -> Int
countIntegers n xs = sum [1 | x <- xs , x == n]
-- countIntegers _ [] = 0
-- countIntegers n (x:xs)
--     | n == x = 1 + countIntegers n xs
--     | otherwise = countIntegers n xs

removeAll :: Int -> [Int] -> [Int]
removeAll _ [] = []
removeAll n (x:xs)
    | n == x = removeAll n xs
    | otherwise = x:removeAll n xs

removeAllButFirst :: Int -> [Int] -> [Int]
removeAllButFirst _ [] = []
removeAllButFirst n (x:xs)
    | n == x = x:removeAll n xs
    | otherwise = x:removeAllButFirst n xs

listMarks :: String -> [StudentMark] -> [Int]
listMarks _ [] = []
listMarks "" _ = []
listMarks name sm = [m | (s,m) <- sm, s == name]
-- listMarks name ((s, m):xs)
--     | name == s = m:listMarks name xs
--     | otherwise = listMarks name xs

test = [i | i <- [1..2], let f = i < 2, f]

sorted :: [Int] -> Bool
sorted xs = andAll [n <= m | (n,m) <- take (length xs - 1) (zip xs (drop 1 xs))]
-- sorted [] = True
-- sorted (x:[]) = True
-- sorted (n:m:s) = n <= m && sorted (m:s)

prefix :: [Int] -> [Int] -> Bool
prefix [] _ = True
prefix _ [] = False
prefix (x:xs) (y:ys) = x == y && prefix xs ys

subSequence :: [Int] -> [Int] -> Bool
subSequence [] [] = True
subSequence xs [] = False
subSequence xs (y:ys)
    | prefix xs (y:ys) = True
    | otherwise = subSequence xs ys