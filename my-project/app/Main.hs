module Main where

--import Lib // error import ???
--import MyChar // error import ???
import Text.Printf (printf)
import Data.List()
import Data.Char

main = do
    print (square 3)
    print (cube 3.4)
    print (quad 3)
    print (capitalize 'h')
    print (myBuildLeft 3 [1,2,3] ++ [4,5,6])
    print (myBuildRight 4 [1,2,3])
    print (fst ("cat", "dog"))
    print (smaller (3, 6))
    print (smaller (6, 5))
    print (greater (3, 6))
    print (greater (6, 5))
    print (smallBig (5,1))
    print (4e3 + 2e-2)
    print (4e3 * 2e-2)
    print (4e3 / 2e-2)
    print (mySpace 'd')

square :: Int -> Int
square x = x * x

cube :: Float -> Float
cube x = x ^ 3

quad :: Int -> Int
quad x = square (square x)

offset :: Int
offset = ord 'A' - ord 'a'

capitalize :: Char -> Char
capitalize ch = chr (ord ch + offset)

myBuildLeft :: Int -> [Int] -> [Int]
myBuildLeft x ls = x : ls 

myBuildRight :: Int -> [Int] -> [Int]
myBuildRight x ls = ls ++ [x] 

smaller :: (Int, Int) -> Int
smaller (x, y) = if x <= y then x else y

greater :: (Int, Int) -> Int
greater (x, y) = if x >= y then x else y

smallBig :: (Int, Int) -> (Int, Int)
smallBig (x, y) = if x <= y then (x , y) else (y, x)

mySpace :: Char -> Bool
mySpace c = c == ' '