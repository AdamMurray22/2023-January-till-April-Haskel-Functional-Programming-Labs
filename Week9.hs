import Text.Printf
import Data.List
import Data.Maybe
import Data.Time
import Text.Read

type Make = String

type Model = String

type NumberPlate = String

data CarName = CName Make Model
  deriving (Show)

type Mileage = Int

type MotYear = Int

type Mots = [MotYear]

data Car = Car NumberPlate CarName Mileage Mots
  deriving (Show)

testCars :: [Car]
testCars =
  [
    Car "AB12 CDE" (CName "Ford" "Fiesta") 71650 [2020, 2021],
    Car "CD34 EFG" (CName "Ford" "Focus") 10354 [2017, 2018, 2019, 2020, 2021, 2022],
    Car "EF56 GHI" (CName "Ford" "Mondeo") 35465 [2019, 2020, 2021, 2022, 2023],
    Car "GH78 IJK" (CName "Vauxhall" "Corsa") 94759 [2020, 2021, 2022],
    Car "IJ90 KLM" (CName "Vauxhall" "Astra") 3964 [2021, 2022],
    Car "KL12 MNO" (CName "Vauxhall" "Vectra") 99801 [2020, 2021, 2022, 2023],
    Car "MN34 OPQ" (CName "Vauxhall" "Vectra") 5554 [2020, 2021, 2022, 2023],
    Car "OP56 QRST" (CName "Volkswagen" "Golf") 65168 [2021, 2022],
    Car "QR78 STU" (CName "Volkswagen" "Golf") 45630 [2020, 2021, 2022, 2023],
    Car "ST90 UVW" (CName "Volkswagen" "Passat") 36325 [2018, 2019]
  ]

getNumberPlate :: Car -> NumberPlate
getNumberPlate (Car numberPlate _ _ _) = numberPlate

getCarByNumberPlate :: String -> [Car] -> Maybe Car
getCarByNumberPlate _ [] = Nothing
getCarByNumberPlate np (car:rest) =
  if np == getNumberPlate car
    then Just car
    else getCarByNumberPlate np rest

getMots :: Car -> Mots
getMots (Car _ _ _ mots) = mots

getCarsThatNeedMotForYear :: Int -> [Car] -> [Car]
getCarsThatNeedMotForYear _ [] = []
getCarsThatNeedMotForYear year (car : rest) =
  if year `notElem` getMots car
    then car : getCarsThatNeedMotForYear year rest
    else getCarsThatNeedMotForYear year rest

formatCar :: Car -> String
formatCar (Car numberPlate (CName make model) mileage motYears) =
  printf "%-10s %-10s %-15s %-10d %-5d"
  make model numberPlate mileage (last motYears)

formatCars :: [Car] -> String
formatCars [] = ""
formatCars (car:rest) = formatCar car ++ "\n" ++ formatCars rest

main1 :: IO ()
main1 = do
  putStrLn "Select one of the following options:"
  putStrLn "1. Get car by number plate"
  putStrLn "2. Get cars that need MOT for a given year"
  putStrLn "Press any other key to exit"
  option <- getLine
  case option of
    "1" -> do
      putStrLn "Enter a number plate:"
      numberPlate <- getLine
      let car = getCarByNumberPlate numberPlate testCars
      case car of
        Nothing -> putStrLn "No car found"
        Just car -> putStrLn (formatCar car)
      main1
    "2" -> do
      putStrLn "Enter a year to check:"
      year <- getLine
      let cars = getCarsThatNeedMotForYear (read year) testCars
      putStrLn "Make Model Number Plate mileage Last MOT"
      putStrLn "---------------------------------------------------------"
      putStrLn (formatCars cars)
      main1
    _ -> return ()

addMOTtoCar :: String -> Int -> [Car] -> [Car]
addMOTtoCar _ _ [] = []
addMOTtoCar numberPlate year (Car np cn mi mots : rest) =
  if numberPlate == np
    then
      if elem year mots
        then Car numberPlate cn mi mots : rest
        else Car numberPlate cn mi (mots ++ [year]) : rest
    else Car numberPlate cn mi mots : addMOTtoCar np year rest

getNewPlateNumber :: IO String
getNewPlateNumber = do
  putStrLn "Enter a number plate:"
  numberPlate <- getLine
  if pnExists numberPlate
    then do
      putStrLn "Number plate already exists"
      getNewPlateNumber
    else return numberPlate
  where
    pnExists numberPlate = isJust (getCarByNumberPlate numberPlate testCars)

main2 :: IO ()
main2 = do
  putStrLn "Select one of the following options:"
  putStrLn "1. Add a new MOT year to a car"
  putStrLn "2. Add a new car"
  putStrLn "Press any other key to exit"
  option <- getLine
  case option of
    "1" -> do
      putStrLn "Enter a number plate:"
      numberPlate <- getLine
      let car = getCarByNumberPlate numberPlate testCars
      case car of
        Nothing -> putStrLn "No car found"
        Just car -> do
          putStrLn "Enter a MOT year:"
          motYear <- getLine
          let updatedCars = addMOTtoCar numberPlate (read motYear) testCars
          writeFile "cars.txt" (intercalate "\n" (map show updatedCars))
      main2
    "2" -> do
      numberPlate <- getNewPlateNumber
      putStrLn "Enter a make:"
      make <- getLine
      putStrLn "Enter a model:"
      model <- getLine
      let car = Car numberPlate (CName make model) 0 []
      appendFile "cars.txt" ("\n" ++ show car)
      main2
    _ -> return ()

data DiaryEntry = DiaryEntry Title Date Content IsReviewed
  deriving (Show)

type Title = String

data Date = Date Year MonthOfYear DayOfMonth
  deriving (Show)

type Content = String

data IsReviewed = Reviewed | NotReviewed
  deriving (Show)

testEntries :: [DiaryEntry]
testEntries = 
  [
    DiaryEntry "Learnt about GitHub" (Date 2023 03 15) "I completed a tutorial on how to use GitHub" Reviewed,
    DiaryEntry "Met with the clients" (Date 2023 03 17) "I got to meet with the clients of the project" Reviewed,
    DiaryEntry "Completed health and safety training" (Date 2023 03 21) "I just completed the health and safety training" NotReviewed,
    DiaryEntry "First staff meeting" (Date 2023 03 23) "I attended my first staff meeting" Reviewed,
    DiaryEntry "Learnt about C" (Date 2023 03 24) "Did some C stuff" NotReviewed
  ]

formatReview :: IsReviewed -> String
formatReview Reviewed = "Reviewed"
formatReview NotReviewed = "Not reviewed"

formatEntry :: DiaryEntry -> String
formatEntry (DiaryEntry title (Date year month day) content reviewed) =
  printf "Title: %s\n%s\n%s, added on %d%d%d"
  title content (formatReview reviewed) year month day

formatDiary :: [DiaryEntry] -> String
formatDiary [] = ""
formatDiary [entry] = formatEntry entry
formatDiary (entry:rest) = formatEntry entry ++ "\n\n" ++ formatDiary rest

readEntry :: String -> DiaryEntry
readEntry s = DiaryEntry "" (Date 2023 02 14) "" Reviewed

readEntrys :: [String] -> [DiaryEntry]
readEntrys = map readEntry