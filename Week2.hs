import System.Win32 (xBUTTON1, cOLOR_ACTIVEBORDER)
import Data.Time.Calendar.MonthDay (monthAndDayToDayOfYear)
heartMonitor :: Int -> Int -> String
heartMonitor age bpm 
    | age > 80 && bpm > 100 = "High heart rate for +80!"
    | age > 60  && bpm > 130 = "High heart rate for 60-80!"
    | age > 40 && bpm > 140 = "High heart rate for 40-60!"
    | age > 20 && bpm > 155 = "High heart rate for 20-40!"
    | age >= 0 && bpm > 170 = "High heart rate for 0-20!"
    | otherwise = "Normal heart rate"

pizzaCalories :: Int -> String -> Float
pizzaCalories diameter toppings = (11.5 + toppingCalories) * area
    where
        area = pi * (fromIntegral diameter / 2) ^ 2
        toppingCalories
            | toppings == "pepperoni" = 6
            | toppings == "tuna" = 4
            | toppings == "veggie" = 2.5
            | otherwise = 0

absolute :: Int -> Int
absolute x 
    | x > 0 = x
    | otherwise = x * (-1)

absoluteTest :: Bool
absoluteTest = (absolute (-10) == 10) && (absolute 0 == 0) && (absolute 10 == 10)

sign :: Int -> Int
sign x
    | x > 0 = 1
    | x == 0 = 0
    | otherwise = -1

signTest :: Bool
signTest = (sign 5 == 1) && (sign 0 == 0) && (sign (-5) == (-1))

howManyEqual :: Int -> Int -> Int -> Int
howManyEqual a b c 
    | a == b && a == c = 3
    | a == b || a == c || b == c = 2
    | otherwise = 0

sumDiagonalLengths :: Float -> Float -> Float -> Float
sumDiagonalLengths length1 length2 length3 = diagonalLengths length1 + diagonalLengths length2 + diagonalLengths length3
    where
        diagonalLengths length = sqrt (2 * length ^ 2)

taxiFare :: Int -> Float
taxiFare distance = 2.2 + distanceCost
    where 
        distanceCost
            | distance <= 10 = fromIntegral distance * 0.5
            | otherwise = 5 + (( fromIntegral distance - 10) * 0.3)

howManyAboveAverage :: Int -> Int -> Int -> Int
howManyAboveAverage a b c = greaterThan a + greaterThan b + greaterThan c
    where
        average = fromIntegral(a + b + c) / 3
        greaterThan :: Int -> Int
        greaterThan x
            | fromIntegral(x) > average = 1
            | otherwise = 0

validDate :: Int -> Int -> Bool
validDate day month
    | day < 0 && day > 31 && month < 0 && month > 12 = False
    | day <= 28 = True
    | month == 2 = False
    | day <= 30 = True
    | month == 4 || month == 6 || month == 9 || month == 11 = False
    | otherwise = True

daysInMonth :: Int -> Int -> Int
daysInMonth month year
    | month == 2 = 28 + leapYear
    | month == 4 || month == 6 || month == 9 || month == 11 = 30
    | otherwise = 31
     where 
        leapYear
            | mod year 4 == 0 = 1
            | otherwise = 0

