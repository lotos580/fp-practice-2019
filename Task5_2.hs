module Task5_2 where 

import Prelude hiding (concat)
-- Зиппер из лекции   

data Zipper a = Zipper [a] [a] --deriving Show

-- Реализуйте экземпляры классов Show и Eq для этого типа

fromList :: [a] -> Zipper a
fromList lst = Zipper [] lst

goRight :: Zipper a -> Zipper a
goRight z@(Zipper _ []) = z
goRight (Zipper l (rh:rt)) = Zipper (rh:l) rt

goLeft :: Zipper a -> Zipper a
goLeft z@(Zipper [] _) = z
goLeft (Zipper (lh:lt) r) = Zipper lt (lh:r) 

putRight :: a -> Zipper a -> Zipper a
putRight x (Zipper l r) = Zipper l (x:r)

putLeft :: a -> Zipper a -> Zipper a
putLeft x (Zipper l r) = Zipper (x:l) r

removeRight :: Zipper a -> Zipper a
removeRight (Zipper l []) = Zipper l []
removeRight (Zipper l (_:rt)) = Zipper l rt

removeLeft :: Zipper a -> Zipper a
removeLeft (Zipper [] r) = Zipper [] r
removeLeft (Zipper (_:lt) r) = Zipper lt r

-- Используя приведённые выше функции, реализуйте функцию конкатенации
-- вставки подсписка в середину и выделения подсписка



lSize :: Zipper a -> Int
lSize   ( Zipper [] _ ) = 0
lSize z@( Zipper _  _ ) = 1 + ( lSize  $ removeLeft  z ) 
rSize :: Zipper a -> Int 
rSize   ( Zipper _ [] ) = 0
rSize z@( Zipper _  _ ) = 1 + ( rSize $ removeRight z )


lList :: Zipper a -> [a] 
lList (Zipper [] _)       = []
lList z@(Zipper (x:xs) _) = x:( lList  $ removeLeft  z ) 
rList :: Zipper a -> [a] 
rList (Zipper _ [])       = []
rList z@(Zipper _ (x:xs)) = x:( rList $ removeRight z )


lListRd :: Zipper a -> [a] 
lListRd (Zipper [] _ )       = []
lListRd z@(Zipper (x:xs) _ ) = ( lListRd $ removeLeft  z ) ++ [x]
rListRd :: Zipper a -> [a] 
rListRd (Zipper _ [] )       = []
rListRd z@(Zipper _ (x:xs) ) = ( rListRd $ removeRight z ) ++ [x]

-- для адекватного присоединения на фукнцию должен подаваться перевёрнутый список
listToLeft :: [a] -> Zipper a -> Zipper a
listToLeft [] z      = z
listToLeft (x:xs) z  = listToLeft   xs $ putLeft  x z

-- для адекватного присоединения на фукнцию должен подаваться перевёрнутый список
listToRight :: [a]-> Zipper a -> Zipper a
listToRight [] z     = z
listToRight (x:xs) z = listToRight xs $ putRight x z 




concat :: Zipper a -> Zipper a -> Zipper a
--concat (Zipper z1 z2) (Zipper i1 i2) = Zipper (z1 ++ i1) (z2 ++ i2) 
concat z1 z2 = listToRight ( rListRd z1 ) ( listToLeft ( lListRd z1 ) z2 ) 


insertManyAt :: Int -> Zipper a -> Zipper a -> Zipper a
--insertManyAt index (Zipper z1 z2) (Zipper i1 i2) = Zipper ( (take index i1) ++ z1 ++ (drop index i1) ) ( (take index i2) ++ z2 ++ (drop index i2) ) 
-- сперва соединяем по очереди оба зипперы с учётом вставки в начало и конец, сначала левые списки, далее правые 
insertManyAt idx z i = ( moveNLeft ( idxCorr idx )                  ( fromList $ drop ( lsizeMinIdx (idxCorr idx) i ) $ lListRd i ) ) 
              `concat` ( moveNLeft ( lSize z )                      ( fromList $ lListRd z ) ) 
              `concat` ( moveNLeft ( lsizeMinIdx (idxCorr idx) i )  ( fromList $ take ( lsizeMinIdx (idxCorr idx) i ) $ lListRd i ) )
              `concat`                                              ( fromList $ take ( rsizeMinIdx (idxCorr idx) i ) $ rList   i )
              `concat`                                              ( fromList $ rList   z ) 
              `concat`                                              ( fromList $ drop ( rsizeMinIdx (idxCorr idx) i ) $ rList   i )
            where 
                moveNLeft   n   z = if n == 0        then z       else moveNLeft (n-1) $ goRight z 
                rsizeMinIdx idx z = if idx > rSize z then rSize z else idx
                lsizeMinIdx idx z = if idx > lSize z then 0       else ( lSize i - idx + 1 )
                idxCorr     idx   = if idx < 0       then 0       else idx

subZipper :: Int -> Int -> Zipper a -> Zipper a
-- subZipper from to (Zipper z1 z2) = Zipper ( (drop from ( dropfromtail (if to > (length z1) then length z1 else length z1 - to) z1 )) ) 
--                                           ( (drop from ( dropfromtail (if to > (length z2) then length z2 else length z2 - to) z2 )) ) 
--                                                  where dropfromtail i xs = if i == 0 then xs else dropfromtail (i-1) (init xs)
subZipper from to z = concat ( rmNonLeft  from $ prepLeft  to ( moveNLeft ( lSize z ) ( fromList $ lListRd z ) ) $ lSize z )
                             $ rmNonRight from $ prepRight to ( fromList $ rList z ) $ rSize z  
        where 
            moveNLeft i z = if i == 0 then z else moveNLeft (i-1) $ goRight z

            prepRight upTo z len = if upTo == 0 then ( Zipper [] [] ) else 
                                        moveNbackToRight upTo $ 
                                        rmNonRight   ( if len == 0 then 0 
                                                  else ( (len - upTo) `mod` len) ) $ moveNtoRight upTo z

            moveNtoRight     i z = if i == 0 then z else moveNtoRight     (i-1) $ goRight     z
            rmNonRight       i z = if i == 0 then z else rmNonRight       (i-1) $ removeRight z
            moveNbackToRight i z = if i == 0 then z else moveNbackToRight (i-1) $ goLeft      z

            prepLeft upTo z len = if upTo == 0 then ( Zipper [] [] ) else 
                                        moveNbackToLeft upTo $
                                        rmNonLeft   ( if len == 0 then 0 
                                                 else ( (len - upTo) `mod` len ) ) $ moveNtoLeft upTo z

            moveNtoLeft     i z = if i == 0 then z else moveNtoLeft       (i-1) $ goLeft      z
            rmNonLeft       i z = if i == 0 then z else rmNonLeft         (i-1) $ removeLeft  z
            moveNbackToLeft i z = if i == 0 then z else moveNbackToLeft   (i-1) $ goRight     z


instance (Show a) => Show (Zipper a) where
    show (Zipper z1 z2) = show z1 ++ " " ++ show z2  
    
instance (Eq a) => Eq (Zipper a) where 
    (Zipper z1 z2) == (Zipper i1 i2) = z1 == i1 && z2 == i2
      