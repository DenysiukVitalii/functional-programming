module Main where

import System.Info
import Data.Bool

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
