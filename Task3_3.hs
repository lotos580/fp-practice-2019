module Task3_3 where

{-
  Задание 3.3
  Множество на основе предикатов
-}

import Data.Functor.Contravariant


newtype PSet a = PSet{ contains :: a -> Bool } 

-- Реализуйте классы Monoid и Functor
-- Объясните в комментариях, почему они реализованы именно так


-- зависит от того что имеется в виду: множество может включать в себя разные предикаты объединяемые через ИЛИ
-- но если предикаты взаимодействуют через через И, то ситуация совсем иная
 

instance Contravariant PSet where
  contramap f (PSet g) = PSet (g . f) 
  -- использован контрвариативный функтор, т.к. обычный не применим в данном случае: (a -> Bool) 
  
  
instance Semigroup (PSet a) where
  -- если множество расширяется с каждым новым предикатом
  a1 <> a2 = PSet{contains=( \x -> contains a1 x || contains a2 x)}
  -- если множество исключающее, т.е. сужающееся с каждым новым предикатом
  --a1 <> a2 = PSet{contains=( \x -> contains a1 x && contains a2 x )}

instance Monoid (PSet a) where
    -- mappend'ы вынесены в Semigroup
  -- если множество расширяется с каждым новым предикатом
  mempty = PSet{contains=(\l -> False)} 
  -- если множество исключающее, т.е. сужающееся с каждым новым предикатом
  --mempty = PSet{contains=(\l -> True)} 
