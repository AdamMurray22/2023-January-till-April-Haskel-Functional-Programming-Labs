import Data.Char
import Distribution.TestSuite (TestInstance(name))

type StudentMark = (String, Int)

betterStudent :: StudentMark -> StudentMark -> String
betterStudent (s1,m1) (s2,m2) 
    | m1 >= m2          = s1
    | otherwise         = s2

marks:: [StudentMark] -> [Int]
marks stmks = [ mk | (st,mk) <- stmks ]

pass :: [StudentMark] -> [String]
pass stmks = [ st | (st,mk) <- stmks, mk >= 40 ]

-- An example list of student marks
testData :: [StudentMark]
testData = [("John", 53), ("Sam", 16), ("Kate", 85), ("Jill", 65),
            ("Bill", 37), ("Amy", 22), ("Jack", 41), ("Sue", 71)]

addPairs :: [(Int,Int)] -> [Int]
addPairs pairList = [ i+j | (i,j) <- pairList ]

minAndMax :: Int -> Int -> (Int,Int)
minAndMax x y 
    | x <= y            = (x,y)
    | otherwise         = (y,x)


sumNumbersBetween :: Int -> Int -> Int
sumNumbersBetween x y = sum [i | i <- [x .. y]]
-- sumNumbersBetween x y
--     | x > y = 0
--     | otherwise = x + sumNumbersBetween (x + 1) y

sumEvenNumbersBetween :: Int -> Int -> Int
sumEvenNumbersBetween x y = sum [i | i <- [x .. y], mod i 2 == 0]
-- sumEvenNumbersBetween x y
--     | x > y = 0
--     | mod x 2 == 0 = x + sumEvenNumbersBetween (x + 2) y
--     | otherwise = sumEvenNumbersBetween (x + 1) y

averageMark :: [StudentMark] -> Float
averageMark [] = 0
averageMark stmks = fromIntegral sumMarks / fromIntegral numberOfStudents
    where
        sumMarks = sum [mk | (_ , mk) <- stmks]
        numberOfStudents = length stmks

sumDifference :: Int -> Int -> (Int,Int)
sumDifference n m = (n + m, n - m)

grade :: StudentMark -> Char
grade (name, mark)
    | mark > 100 || mark < 0 = error "Invalid Mark"
    | mark >= 70 = 'A'
    | mark >= 60 = 'B'
    | mark >= 50 = 'C'
    | mark >= 40 = 'D'
    | otherwise = 'F'

capMark :: StudentMark -> StudentMark
capMark (name, mark)
    | mark > 100 || mark < 0 = error "Invalid Mark"
    | mark > 40 = (name, 40)
    | otherwise = (name, mark)

firstNumbers :: Int -> [Int]
firstNumbers n = [i | i <- [1 .. n]]

firstSquares :: Int -> [Int]
firstSquares n = [i ^ 2 | i <- firstNumbers n]      

capitalise :: String -> String
capitalise s = [Data.Char.toUpper c | c <- s]

onlyDigits :: String -> String
onlyDigits s = [i | i <- s, Data.Char.isDigit i]

capMarks :: [StudentMark] -> [StudentMark]
capMarks sm = [capMark nsm | nsm <- sm]

gradeStudents :: [StudentMark] -> [(String, Char)]
gradeStudents studentMarks = [(name, grade (name, mark)) | (name, mark) <- studentMarks]

-- duplicate :: String -> Int -> String
-- duplicate s n
--     | n <= 0 = []
--     | n == 1 = s
--     | otherwise = s ++ duplicate s (n - 1)

duplicate :: String -> Int -> String
duplicate str n = [s | i <- [1..n], s <- str]

divisors :: Int -> [Int]
divisors n
    | n <= 0 = []
divisors n = [i | i <- [1..n], mod n i == 0]

isPrime :: Int -> Bool
isPrime n = length (divisors n) == 2

split :: [(a,b)] -> ([a],[b])
split l = ([a | (a,b) <- l], [b | (a,b) <- l])

-- split :: [(a,b)] -> ([a],[b])
-- split l = ([fst t | t <- l], [snd t | t <- l])