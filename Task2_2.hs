module Task2_2 where

{-
  Задание 2.2
  Необходимо реализовать функции foldl, foldr и unfoldr, а также все остальные функции
  в данном файле _на основе этих трёх_
-}

import Todo(todo)

import Prelude hiding (foldl, foldr, unfoldr, map, concatMap,
    filter, maxBy, minBy, reverse, sum, product, elem)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl func arg [] = arg
foldl func arg (x:xs) = foldl func arg' xs 
                          where arg' = arg `func` x

foldr :: (a -> b -> b) -> b -> [a] -> b 
foldr func arg [] = arg
foldr func arg (x:xs) = x `func` foldr func arg xs

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr func arg = case func arg of
                     Nothing -> [] 
                     Just (el, arg') -> el : unfoldr func arg' 


-- Сумма всех элементов списка (пример)
sum :: [Integer] -> Integer
sum lst = foldl (+) 0 lst

-- Переворот списка (Пример)
reverse :: [a] -> [a]
reverse lst = foldl f [] lst where f t h = h:t


-- Отображение элементов списка
map :: (a -> b) -> [a] -> [b] 
map args xs = foldr (\x xs -> args x : xs) [] xs 

-- Произведение всех элементов списка
product :: [Integer] -> Integer
product list = foldr (*) 1 list

-- Выделение из списка Maybe всех существующих значений
catMaybes :: [Maybe a] -> [a]
catMaybes list = foldr (\x list -> case x of
                                      Nothing -> list
                                      Just x  -> x : list ) [] list

-- Диагональ матрицы
diagonal :: [[a]] -> [a] 
diagonal l = reverse (foldr (\x acc -> acc ++ [(x !! (length l - length acc - 1))]) [] l) 

-- Фильтр для всех элементов, не соответствующих предикату
filterNot :: (a -> Bool) -> [a] -> [a]
filterNot pred list = foldr (\x list -> if not (pred x) then x:list else list) [] list

-- Поиск элемента в списке
elem :: (Eq a) => a -> [a] -> Bool
elem el list = foldr (\x acc -> acc || el == x) False list 

-- Список чисел в диапазоне [from, to) с шагом step
rangeTo :: Integer -> Integer -> Integer -> [Integer] 
rangeTo from to step = foldr (\x list -> if mod (x-from) step == 0 then x:list else list) [] [from..(to-1)]

--  Конкатенация двух списков
append :: [a] -> [a] -> [a]
append xs ys = foldr (:) ys xs

-- Разбиение списка lst на куски размером n
-- (последний кусок может быть меньше)
groups :: [a] -> Integer -> [[a]] 
groups lst n = unfoldr (\xs -> if null xs then Nothing 
                                          else Just (splitAt n xs)) lst
