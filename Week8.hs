import Distribution.TestSuite (TestInstance(name))
import GHC.IO.IOMode (IOMode(ReadWriteMode))
helloWorld :: IO ()
helloWorld = putStrLn "Hello, World!"

displayFile :: IO ()
displayFile = do 
    putStr "Enter the filename: "
    name <- getLine
    contents <- readFile name
    putStr contents

getInt :: IO Int
getInt = do 
    str <- getLine
    return (read str :: Int)

isPalindrome :: String -> String
isPalindrome str
   | str == reverse str  = str ++ " is a palindrome"
   | otherwise           = str ++ " is not a palindrome"

pal :: IO ()
pal = do 
    line <- getLine
    let response = isPalindrome line
    putStrLn response

palLines :: IO ()
palLines = do 
    putStr "Enter a line: "
    str <- getLine
    if str == "" then 
        return ()
    else do 
        putStrLn (isPalindrome str)
        palLines


helloName :: IO ()
helloName = do
        putStr "Enter your name: "
        str <- getLine
        putStrLn ("Hello " ++ str)

addTwoNumbers :: IO ()
addTwoNumbers = do
        putStr "Enter the first number: "
        str1 <- getLine
        let num1 = read str1 :: Int
        putStr "Enter the second number: "
        str2 <- getLine
        let num2 = read str2 :: Int
        let sum = show (num1 + num2) :: String
        putStrLn (str1 ++ " + " ++ str2 ++ " = " ++ sum)

copyFile :: IO ()
copyFile = do
        putStr "Enter the filename to copy from: "
        fromName <- getLine
        contents <- readFile fromName
        putStr "Enter the filename to copy to: "
        toName <- getLine
        writeFile toName contents

listBuilder :: IO ()
listBuilder = buildList []


buildList :: [String] -> IO ()
buildList xs = do
        putStr "Enter a line: "
        str <- getLine
        if str == "" then
            return()
        else do
            buildList (str:xs)

sumOf :: IO ()
sumOf = do
    putStr "Enter how many numbers you want to sum: "
    str <- getLine
    let n = read str :: Int
    sumOfN n 0

sumOfN :: Int -> Int -> IO ()
sumOfN n sum = do
        if n <= 0 then
            putStrLn (show sum)
        else do
            putStr "Enter a number: "
            str <- getLine
            let num = read str :: Int
            sumOfN (n - 1) (sum + num)    

addWord :: String -> [String] -> [String]
addWord str xs = reverse (str : (reverse xs))

wordsToString :: [String] -> String
wordsToString (x:xs) = listToString x xs

listToString :: String -> [String] -> String
listToString str [] = str
listToString "" (x:xs) = listToString x xs
listToString str (x:xs) = listToString (str ++ "\n" ++ x) xs

wordsOfLength :: Int -> [String] -> [String]
wordsOfLength n xs = [i | i <- xs, length i == n]

main :: IO ()
main = do
        content <- readFile "words.txt"
        let fileStrings = read content :: [String]
        menu fileStrings

menu :: [String] -> IO ()
menu fileStrings = do
        putStr "Enter 1 for adding a word to the list\n2 to display all words\n3 to display all words of a given length\n4 to exit\n"
        i <- getLine
        if i == "1" then do
            strings <- addGivenWord fileStrings
            menu fileStrings
        else if i == "2" then do
            displayAllWords fileStrings
            menu fileStrings
        else if i == "3" then do
            displayAllWordsOfLength fileStrings
            menu fileStrings
        else if i == "4" then
            return ()
        else do
            putStrLn "Invalid option"
            menu fileStrings

addGivenWord :: [String] -> IO ()
addGivenWord fileStrings = do
        putStr "Enter a word: "
        str <- getLine
        let strings = addWord str fileStrings
        writeFile "words.txt" (show strings)

displayAllWords :: [String] -> IO ()
displayAllWords fileStrings = do
        let str = wordsToString fileStrings
        putStrLn str

displayAllWordsOfLength :: [String] -> IO ()
displayAllWordsOfLength fileStrings = do
        putStr "Enter the lenth of words you want to see: "
        lngth <- getLine
        let length = read lngth :: Int
        let strings = wordsOfLength length fileStrings
        let str = wordsToString strings
        putStrLn str