module Main where

import System.Info
import Data.Bool
import Text.Show

main :: IO ()
main = do
    print (solve2 2.5 2.3)
    print (max3 2 4 1)
    print (sort2 4 2)
    print (isParallel (1,1) (2,2) (2,0) (4,2))
    print (isSorted 1 2 3)

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

includedEvery :: [Shape]
includedEvery = [ s | s <- database, (surface s) > 10]


sum' :: [Integer] -> Integer
sum' [] = 0
sum' (x:xs) = x + sum' xs

getPartHalf :: [Integer] -> [Integer]
getPartHalf xs = take ((length xs) `div` 2) xs

findSum' :: [Integer] -> Integer -> Integer
findSum' xs n = sum' [x | x <- getPartHalf xs, x == n]

findSum'' :: [Integer] -> Integer -> Integer
findSum'' xs n = sum' ( filter ( == n) (getPartHalf xs) )
