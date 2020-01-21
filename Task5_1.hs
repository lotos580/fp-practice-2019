module Task5_1 where

import Todo(todo)

-- Структура двусвязного списка из лекции про ленивость

data DList a = DNil 
             | DCons { 
                left :: (DList a), 
                current :: a, 
                right :: (DList a) 
             } --deriving Show

instance (Show a) => Show (DList a) where
    show it = "[" ++ showBody it ++ "]"
              where showBody DNil = ""
                    showBody (DCons _ h DNil) = show h
                    showBody (DCons _ h t) = show h ++ ", " ++ showBody t

instance (Eq a) => Eq (DList a) where
    DNil == DNil = True
    (DCons _ h1 t1) == (DCons _ h2 t2) = h1 == h2 && t1 == t2
    _ == _ = False

list2dlist :: [a] -> DList a
list2dlist lst = list2dlist' DNil lst

list2dlist' :: DList a -> [a] -> DList a
list2dlist' _ [] = DNil
list2dlist' left (h: t) = 
    let rec = DCons left h (list2dlist' rec t)
    in rec


-- Реализуйте функции индексирования, вставки и удаления элементов
index :: DList a -> Int -> a
index DNil i = error "index mismatch."
index dl i = if i == 0 then current dl else 
                                        if i > 0 then index (right dl) (i - 1)
                                                 else index ( left dl) (i + 1)
    

insertAt :: DList a -> Int -> a -> DList a 
insertAt DNil i v 
            | i == 0 = DCons{left=DNil, current=v, right=DNil}
            | otherwise = error "index mismatch."
insertAt (DCons l v r) i val  
            | otherwise  = (DCons{left=l, current=v, right=( insertAt (r) (i - 1) val )})


            
keepTheList :: [DList a] -> DList a -> [DList a] 
keepTheList xs DNil = xs
keepTheList xs (DCons l v r) = xs ++ [(DCons l v r)] ++ (keepTheList [] (r))
 
 

removeAt :: DList a -> Int -> DList a
removeAt DNil _ = DNil
removeAt (DCons l _ DNil)  0 = DCons{left=(left l), current=(current l), right=DNil}   --  конечный элемент
removeAt (DCons DNil _ r)  0 = DCons{left=DNil, current=(current r), right=(right r)}  --  начальный элемент 
removeAt (DCons l v r) i = if i == 0 then DCons{left=l, current=(current r), right=(right r)}  
                                     else DCons{left=l, current=v, right=(removeAt r (i - 1))}   --  поиск элемента и замена

 
