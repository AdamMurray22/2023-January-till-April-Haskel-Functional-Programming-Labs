sideOfCylinder :: Float -> Float -> Float
sideOfCylinder d h = circumferenceOfCircle d * h

circumferenceOfCircle :: Float -> Float
circumferenceOfCircle d = pi * d

all3CanDrink :: Int -> Int -> Int -> Bool
all3CanDrink a b c = canDrink a && canDrink b && canDrink c

canDrink :: Int -> Bool
canDrink a = a >= 18

timesTen :: Int -> Int
timesTen x = 10 * x

sumThree :: Int -> Int -> Int -> Int
sumThree x y z = x + y + z

areaOfCircle :: Float -> Float
areaOfCircle r = pi * r ^ 2

volumeOfCylinder :: Float -> Float -> Float
volumeOfCylinder r h = areaOfCircle r * h

distance :: Float -> Float -> Float -> Float -> Float
distance x1 y1 x2 y2 = sqrt ((y1 - y2)^2 + (x1 - x2)^2)

threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent x y z = x /= y && different y z && different x z

different :: Int -> Int -> Bool
different x y = x /= y

divisibleBy :: Int -> Int -> Bool
divisibleBy x y = mod x y == 0

isEven :: Int -> Bool
isEven x = divisibleBy x 2

averageThree :: Int -> Int -> Int -> Float
averageThree x y z = fromIntegral (sumThree x y z) / 3

absolute :: Int -> Int
absolute x = if x >= 0 then x else -x




testEverything :: Bool
testEverything = timesTenTest && sumThreeTest && areaOfCircleTest && volumeOfCylinderTest && distanceTest && threeDifferentTest && divisibleByTest && isEvenTest && averageThreeTest && absoluteTest

timesTenTest :: Bool
timesTenTest = (timesTen 5 == 50) && (timesTen 60 == 600)

sumThreeTest :: Bool
sumThreeTest = sumThree 12 9 20 == 41

areaOfCircleTest :: Bool
areaOfCircleTest = areaOfCircle 4 == 50.265484

volumeOfCylinderTest :: Bool
volumeOfCylinderTest = volumeOfCylinder 9 10 == 2544.6902

distanceTest :: Bool
distanceTest = (distance 9 15 5 12 == 5.0) && (distance 1.5 1.75 (-1.25) (-1.50) == 4.2573466)

threeDifferentTest :: Bool
threeDifferentTest = threeDifferent 10 11 12 && not(threeDifferent 10 12 12)

divisibleByTest :: Bool
divisibleByTest = divisibleBy 10 2 && not(divisibleBy 10 3)

isEvenTest :: Bool
isEvenTest = isEven 10 && not(isEven 11)

averageThreeTest :: Bool
averageThreeTest = (averageThree 68 72 56 == 65.333336) && (averageThree 10 11 12 == 11.0)

absoluteTest :: Bool
absoluteTest = (absolute (-10) == 10) && (absolute 0 == 0) && (absolute 10 == 10)