module Task3_2 where

{-
  Задание 3.2
  Структура данных "перевёрнутый список" представляет собой зеркально обращённый
  односвязный список из стандартной библиотеки, для которого взятие последнего
  элемента является наиболее эффективной операцией.
  Необходимо реализовать все функции и классы типов в данном файле.
-}

import Todo(todo)

data ReverseList a = RNil | RCons (ReverseList a) a -- deriving Show

rlistToList :: ReverseList a -> [a]
rlistToList RNil = []
rlistToList (RCons a b) = [b] ++ (rlistToList a) 

listToRList :: [a] -> ReverseList a
listToRList [] = RNil
listToRList (x:xs) = RCons (listToRList xs) x

-- Реализуйте классы Eq, Ord, Show, Monoid, Functor

-- в качестве общей проверки использовалось:
-- > rlistToList (listToRList [1,2,3] <> fmap (+3) (listToRList [1,2,3])) == ([1,2,3] <> fmap (+3) ( [1,2,3])) 
-- > (listToRList [1,2,3] <> fmap (+3) (listToRList [1,2,3]))
-- > reverse ([1,2,3] <> fmap (+3) ( [1,2,3]))


instance (Eq a) => Eq (ReverseList a) where
  a1 == a2 = rlistToList (a1) == rlistToList (a2) 
    
instance (Ord a) => Ord (ReverseList a) where 
  a1 `compare` a2 = rlistToList (a1) `compare` rlistToList (a2)
 
instance (Show a) => Show (ReverseList a) where 
  -- show (RCons a1 a2) = show $ reverse $ rlistToList (RCons a1 a2) -- примитивный вариант
  show me = "[" ++ showBody me ++ "]"
              where showBody RNil = ""
                    showBody (RCons RNil a) = show a
                    showBody (RCons b a) = show a ++ "," ++ showBody b




instance Semigroup (ReverseList a) where   
  RCons RNil a1 <> RCons RNil a2 = RCons ( RCons RNil a2 ) a1
  RCons ( RCons b1 b2 ) a1 <> RCons RNil a2 = RCons ( RCons b1 b2 <> RCons RNil a2 ) a1
  RCons RNil a1 <> RCons ( RCons b1 b2 ) a2 = RCons ( RCons RNil a2 ) a1 <> ( RCons b1 b2 )
  RCons ( RCons b1 b2 ) a1 <> RCons ( RCons c1 c2 ) a2 = RCons ( RCons b1 b2 <> RCons ( RCons c1 c2 ) a2 ) a1   

instance Monoid (ReverseList a) where 
    mempty = RNil 
    -- mappend реализован в полугруппе выше


instance Functor ReverseList where 
    fmap _ RNil = RNil 
    fmap f (RCons a1 a2) = RCons (fmap f a1) (f a2)
