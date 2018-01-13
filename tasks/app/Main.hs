module Main where

-- 5. Знайти суму елементів, значення яких менше голови списку.

-- Фільтруємо список => у списку залишаються значення, які менші за голову списку
-- Використовуємо встроєну функцію суми
-- При необхідності, використовуємо власну функцію знаходження суми елементів списку

-- Рішення
sumListHead :: [Int] -> Int
sumListHead (x:xs) = sum (filter (<x) xs)

-- Функція знаходження суми елементів списку
sum' :: [Integer] -> Integer
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- Приклади

-- sumListHead [1,1,2,3] -> 0
-- sumListHead [4,1,2,3,4,5] -> 6
-- sumListHead [4,3,4,5,2] -> 5

------------------------------------------------

-- 6. Знайти суму елементів, значення яких менше останнього елемента списку.

-- Фільтруємо список => у списку залишаються значення, 
-- які менші за останній елемент - функція last
-- Використовуємо встроєну функцію суми
-- При необхідності, використовуємо власну функцію знаходження суми елементів списку

-- Рішення
sumListLast :: [Int] -> Int
sumListLast list = sum (filter (<last list) list)

-- Приклади

-- sumListLast [1,1,2,3] -> 4
-- sumListLast [6,2,4,5] -> 6
-- sumListLast [4,3,4,5,1] -> 0

-------------------------------------------------

-- 7. Видалити зі списку i-те входження (всі входження) вказаного 
--    значення елемента з першої половини списку.

-- Формуємо нову першу половину списку, де перебираємо 
-- першу половину списку(firstHalf), значення якого
-- порівнюємо з вхідним значенням(value)
-- Після цього робимо конкатенацію з другою половиною списку(secondHalf)

firstHalf :: [Int] -> [Int]
firstHalf xs = take ((length xs) `div` 2) xs

secondHalf :: [Int] -> [Int]
secondHalf xs = reverse (take ((length xs) `div` 2) (reverse xs))

-- Рішення
delElemFirstHalf :: [Int] -> Int -> [Int]
delElemFirstHalf list value = [y | y <- firstHalf list, y /= value] ++ secondHalf list

delElemFirstHalfFilter :: [Int] -> Int -> [Int]
delElemFirstHalfFilter list value = filter (/=value) (firstHalf list) ++ secondHalf list

-- Приклади

-- delElemFirstHalf [1,2,2,3,3,2,2,1] 2 -> [1,3,3,2,2,1]
-- delElemFirstHalf [1,2,3,4] 1 -> [2,3,4]
-- delElemFirstHalf [1,2,2,3] 3 -> [1,2,2,3]

-------------------------------------------------

-- 8. Видалити зі списку i-те входження (всі входження) елемента 
--    із значеннями з указаного діапазону.

-- Формуємо новий список. 
-- Використовуємо предикат, який перевіряє чи не належить елемент
-- заданому діапазону (a, b).
-- 1 аргумент - список
-- 2 аргумент - діапазон у вигляді кортежу

-- Рішення
delElemRange :: [Int] -> (Int, Int) -> [Int]
delElemRange list (a, b) = [y | y <- list, y < a || y > b]

delElemRangeFilter :: [Int] -> (Int, Int) -> [Int]
delElemRangeFilter list (a, b) = filter (\ i -> i < a || i > b) list

-- Приклади

-- delElemRange [1,2,3,4,5,6] (3,5) -> [1,2,6]
-- delElemRange [5,3,1,5,7,2,1,4] (1,3) -> [5,5,7,4]
-- delElemRange [1,2,3,4,5,6] (13,15) -> [1,2,3,4,5,6]
-- delElemRange [1,2,3,4,5,6] (3) -> Error: (a, b) - true