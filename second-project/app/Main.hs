{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Info
import Data.Bool
import Text.Show
import Data.Typeable


import Database.MySQL.Simple 
import Control.Monad
import qualified Data.Text as Text

main :: IO ()
main = do
    conn <- connect defaultConnectInfo { 
      connectUser = "root", 
      connectPassword = "1111", 
      connectDatabase = "quizzzyDB"
    }
    -- execute conn "insert into users (username, role, password) values (?, ?, ?)" 
    --              ["Mike" :: String, "student" :: String, "qwerty123" :: String]
    --execute conn "delete from users where username = ?" ["Mike":: String]
    users <- query_ conn "select username, password from users"
    forM_ users $ \(username, password) ->
        putStrLn $ Text.unpack username ++ " " ++ Text.unpack password
    close conn


solve2 :: Double -> Double -> (Bool, Double)
solve2 a b | (-b / a /= 0 && (b /= 0.0 && a /= 0.0)) = (True, -b / a)
           | otherwise = (False, 0.0)

max3 :: Int -> Int -> Int -> Int
max3 a b c = maximum [a,b,c] 

min3 :: Int -> Int -> Int -> Int
min3 a b c = minimum [a,b,c] 

sort2 :: Int -> Int -> (Int, Int)
sort2 a b | a > b = (b, a)
          | otherwise = (a, b)

isParallel :: (Float, Float) -> (Float, Float) -> 
              (Float, Float) -> (Float, Float) -> Bool
isParallel (x1, y1) (x2, y2) 
           (x3, y3) (x4, y4) | ((y2 - y1) / (x2 - x1)) == (y4 - y3) / (x4 - x3) = True
                             | otherwise = False

isSorted :: Int -> Int -> Int -> Bool
isSorted a b c | a < b && b < c = True  
               | otherwise = False

cutList :: [Int] -> [Int]
cutList xs = init $ tail xs

isPalindrome' :: [Int] -> Bool
isPalindrome' xs = xs == (reverse xs)

isPalindrome :: [Int] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome xs  = (head xs) == (last xs) && (isPalindrome (init $ tail xs))

splitAt' :: [Char] -> Int -> ([Char], [Char])
splitAt' str n = (take n str, drop n str)

sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (x:xs) = x : (sieve $ filter (\ y -> (y `mod` x) /= 0) xs )
 
factorize :: Integer -> [Integer]
factorize n = filter (\ x -> (n `mod` x)==0) $ sieve [2,3..n `div` 2]
 
--task :: Integer -> [Integer]
--task n = map (\ f -> (f)) $ factorize n

isPrime :: Integer -> Bool
isPrime x = divisors x == [1,x]

divisors :: Integer -> [Integer]
divisors 1 = [1]
divisors x = 1:[ y | y <- [2..(x `div` 2)], x `mod` y == 0] ++ [x]

is_prime :: Integer -> Bool
is_prime 1 = False  
is_prime 2 = True
is_prime n | (length [x | x <- [2 .. n-1], n `mod` x == 0]) > 0 = False
           | otherwise = True

task :: Integer -> [Integer]
task n = filter (\x -> if (is_prime x) then True else False) (divisors n)


is_prime' :: Integer -> Bool
is_prime' n = length (divisors n) == 2 

mySum :: [Integer] -> Integer -> Integer -> Integer
mySum xs a b = sum (filter (\x -> if (x >= a && x <= b) then True else False) xs)



data Font = Consolas | LucidaConsole | SourceCodePro deriving (Show)  
data Point = Point Float Float deriving (Show)
--data Point = Point {x :: Float, y :: Float} deriving (Show)
data Shape = Circle Point Float | 
             Rectangle Point Point | 
             Triangle Point Point Point |
             Label Point Font String deriving (Show)  

surface :: Shape -> Float  
surface (Circle _ r) = pi * r ^ 2  
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)
surface (Triangle (Point x1 y1) (Point x2 y2) (Point x3 y3)) = 
        0.5 * ((x1*y2 + x2*y3 + x3*y1)-(y1*x2 + y2*x3 + y3*x1))

database :: [Shape]
database = [(Circle (Point 2 5) 5), (Circle (Point 1 4) 3), (Circle (Point 8 3) 4),
            (Rectangle (Point 0 5) (Point 10 0)), (Rectangle (Point 3 5) (Point 10 0)),(Rectangle (Point 0 10) (Point 20 0)),
            (Triangle (Point 1 1) (Point 2 2) (Point 3 1)), (Triangle (Point 2 5) (Point 5 8) (Point 9 1))]

includedEvery :: Float -> [Shape]
includedEvery n = filter (\ e -> if (surface e > n) then True else False) database

data Date = Date {day :: Integer, month :: Integer} deriving (Show) 
data Meeting = Meeting {date :: Date, place :: String, describe :: String, wasMeeting :: Bool} deriving (Show) 
data Note = Note { name :: String, phone :: [String], birthday :: Date, meetings :: [Meeting] } deriving (Show) 

notebook :: [Note]
notebook = [Note { name = "Peter", 
                   phone = ["+1 492-223-2780"], 
                   birthday = Date {day = 3, month = 11},
                   meetings = [ Meeting { date = Date {day = 5, month = 10}, 
                                           place = "New York", 
                                           describe = "Near Manhattan", 
                                           wasMeeting = True},
                                Meeting { date = Date {day = 29, month = 12}, 
                                           place = "Las Vegas", 
                                           describe = "Near Hotel Bellagio", 
                                           wasMeeting = False}]},
            Note { name = "John", 
            phone = ["+1 323-798-1670"], 
            birthday = Date {day = 15, month = 3},
            meetings = [ Meeting { date = Date {day = 16, month = 5}, 
                                    place = "New York", 
                                    describe = "Near Empire State", 
                                    wasMeeting = True},
                        Meeting { date = Date {day = 4, month = 9}, 
                                    place = "Los Angeles", 
                                    describe = "Near Tussauds's Museum", 
                                    wasMeeting = False}]}
            ]

findFirstLetterName :: Char -> [Note]
findFirstLetterName s = filter (\ e -> (name e)!!0 == s) notebook

flatten :: [[a]] -> [a]         
flatten xs = (\z n -> foldr (\x y -> foldr z y x) n xs) (:) []

getMeetings :: [Meeting]
getMeetings = flatten (map (\i -> meetings i) notebook)

noMeetings :: [Meeting]
noMeetings = filter (\i -> wasMeeting i == False) getMeetings

-- ///////////////////////////////////////////////////////////

sum' :: [Integer] -> Integer
sum' [] = 0
sum' (x:xs) = x + sum' xs

getPartHalf :: [Integer] -> [Integer]
getPartHalf xs = take ((length xs) `div` 2) xs

findSum' :: [Integer] -> Integer -> Integer
findSum' xs n = sum' [x | x <- getPartHalf xs, x == n]

findSum'' :: [Integer] -> Integer -> Integer
findSum'' xs n = sum' ( filter ( == n) (getPartHalf xs) )
