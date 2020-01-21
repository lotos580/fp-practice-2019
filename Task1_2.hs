module Task1_2 where

{-
  Задание 1.2
  Необходимо реализовать четыре любые функции в данном файле
-}

import Todo(todo)

import Prelude hiding (gcd)

-- синус числа (формула Тейлора)
sin :: Double -> Double
sin x = todo --left

-- косинус числа (формула Тейлора)
cos :: Double -> Double
cos x = todo --left

-- наибольший общий делитель двух чисел
gcd :: Integer -> Integer -> Integer
gcd x 0 = abs x
gcd x y = gcd b (mod a b)
    where a = x
          b = y 

-- XXX существует ли полный целочисленный квадрат в диапазоне [from, to)?
doesSquareBetweenExist :: Integer -> Integer -> Bool
doesSquareBetweenExist from to = todo --left

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year = todo --left

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
pow :: Integer -> Integer -> Integer
pow x 0 = 1
pow x y = x * pow x ( y - 1 )

-- является ли данное число простым?
isPrime :: Integer -> Bool 
isPrime 1 = False
isPrime 2 = True 
isPrime num 
       | ( length [x | x <- [2 .. (num - 1)], mod num x == 0] ) > 0 = False
       | otherwise = True
       

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea points = todo --left

-- треугольник задан своими координатами.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Point2D -> Point2D -> Point2D -> Integer
triangleKind a b c 
          | ((fst b - fst a)) * ((snd c - snd a)) - ((fst c - fst a)) * ((snd b - snd a)) == 0 = -1
          | ( hyp ^ 2) > ( cath1 ^ 2) + ( cath2 ^ 2) = 0
          | ( hyp ^ 2) < ( cath1 ^ 2) + ( cath2 ^ 2) = 1
          | ( hyp ^ 2) == ( cath1 ^ 2) + ( cath2 ^ 2) = 2 
          where side1 = sqrt (((fst b - fst a) ^ 2) + ((snd b - snd a) ^ 2)) 
                side2 = sqrt (((fst c - fst b) ^ 2) + ((snd c - snd b) ^ 2)) 
                side3 = sqrt (((fst a - fst c) ^ 2) + ((snd a - snd c) ^ 2)) 
                hyp = foldr1 (\x y ->if x >= y then x else y) [side1, side2, side3]
                cath1 = minimum [ x | x <- [side1, side2, side3], x /= hyp ]
                cath2 = foldr1 (\x y ->if x >= y then x else y) [ x | x <- [side1, side2, side3], x /= hyp ]