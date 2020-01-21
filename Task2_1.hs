module Task2_1 where

{-
  Задание 2.1
  На основе структуры бинарного дерева из лекции необходимо реализовать свою структуру данных
  бинарного дерева поиска (без балансировки) и все операции, приведённые в данном файле
-}

import Todo(todo)

import Prelude hiding (lookup)

-- Ассоциативный массив на основе бинарного дерева поиска
-- Ключи - Integer, значения - произвольного типа
--data TreeMap v = ChangeMe
data TreeMap v = EmptyTree | Node { key :: Integer, value :: v, l_branch :: (TreeMap v) , r_branch :: (TreeMap v)}
                deriving (Show, Eq, Ord)

                
-- Пустое дерево
emptyTree :: TreeMap v
emptyTree = EmptyTree

-- Содержится ли заданный ключ в дереве?
contains :: TreeMap v -> Integer -> Bool
contains t k = case t of
                  EmptyTree -> False
                  Node{} -> if key t == k then True else ( contains (l_branch t) k || contains (r_branch t) k)


-- Значение для заданного ключа
lookup :: (Eq v ) =>Integer -> TreeMap v -> v
lookup k t  = case t of 
                EmptyTree -> error "no such key"
                Node{} -> if key t == k then (value t) else 
                              if (l_branch t) /= EmptyTree then (lookup k (l_branch t)) 
                                                           else (lookup k (r_branch t))  


-- Вставка пары (ключ, значение) в дерево
insert :: (Eq v, Ord v) => (Integer, v) -> TreeMap v -> TreeMap v
insert (k, v) t = case t of 
                EmptyTree -> Node k v EmptyTree EmptyTree
                Node{} -> if value t == v then t{l_branch=(insert (k, v) (l_branch t))}  else 
                                                  if ((value t) < v) then t{l_branch=(insert (k, v) (l_branch t))} 
                                                                     else t{r_branch=(insert (k, v) (r_branch t))}

-- Удаление элемента по ключу
remove :: Integer -> TreeMap v -> TreeMap v
remove i t = case t of 
              EmptyTree -> EmptyTree
              Node{} -> if key t == i then EmptyTree else t{l_branch=(remove i (l_branch t)), r_branch=(remove i (r_branch t))}

-- Поиск ближайшего снизу ключа относительно заданного
nearestLE :: (Eq v) => Integer -> TreeMap v -> (Integer, v)
nearestLE i t = if not (contains t i) then error "no such key there" else getNearestKeyData (getNode i t)  

                where 
                  getNode :: (Eq v) => Integer -> TreeMap v -> TreeMap v
                  getNode k tree = case tree of 
                            EmptyTree -> error "no such key there"
                            Node{} -> if (key tree) == k then tree else 
                                                          if (l_branch tree) /= EmptyTree then getNode k (l_branch tree) 
                                                                                          else getNode k (r_branch tree) 
                                                                                           
                  getNearestKeyData tree 
                        | (l_branch tree) /= EmptyTree = (key (l_branch tree) , value (l_branch tree) )
                        | (r_branch tree) /= EmptyTree = (key (r_branch tree) , value (l_branch tree) )
                        | otherwise = error "no such key there" 

-- Построение дерева из списка пар
treeFromList :: (Ord v) => [(Integer, v)] -> TreeMap v
treeFromList lst = foldr insert emptyTree lst   


-- Построение списка пар из дерева
listFromTree :: TreeMap v -> [(Integer, v)]
listFromTree t = case t of 
            EmptyTree -> []
            Node{} -> listFromTree (l_branch t) ++ [(key t, value t)] ++ listFromTree (r_branch t)

-- Поиск k-той порядковой статистики дерева
kMean :: (Ord v) => Integer -> TreeMap v -> (Integer, v)
kMean i t = listform !! (fromInteger ( i - 1 ))
            where quicksort [] = []  
                  quicksort (x:xs) =   
                      let smallerSorted = quicksort [a | a <- xs, (snd a) <= (snd x)]  
                          biggerSorted = quicksort [a | a <- xs, (snd a) > (snd x)]  
                      in  smallerSorted ++ [x] ++ biggerSorted  

                  listform = quicksort (listFromTree t)