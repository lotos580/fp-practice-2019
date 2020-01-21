module Task3_1 where

{-
  Задание 3.1
  Числа Пеано: представление чисел на основе нуля и операций "+1" и "-1".
  Необходимо реализовать все классы типов, характерные для целых чисел.
-}

data WeirdPeanoNumber = Zero | Succ WeirdPeanoNumber | Pred WeirdPeanoNumber deriving Show

-- Реализуйте все классы типов, которым должны отвечать целые числа   


wpnToInt :: WeirdPeanoNumber -> Integer
wpnToInt Zero = 0
wpnToInt (Succ a) = 1 + wpnToInt a 
wpnToInt (Pred a) = (-1) + wpnToInt a

wpnFromInteger :: Integer -> WeirdPeanoNumber 
wpnFromInteger i 
  | i == 0 = Zero
  | i > 0 = Succ (wpnFromInteger (i - 1))
  | i < 0 = Pred (wpnFromInteger (i + 1))


  
instance Num WeirdPeanoNumber where 
  Zero + b = b
  (Succ a) + b = Succ (a + b)

  a - b = wpnFromInteger ( wpnToInt (a) - wpnToInt (b)) 

  a * b = wpnFromInteger ( wpnToInt (a) * wpnToInt (b)) 

  negate Zero = Zero
  negate (Succ a) = Pred (negate a)
  negate (Pred a) = Succ (negate a)

  abs Zero = Zero
  abs (Succ a) = Succ (abs a)
  abs (Pred a) = Succ (abs a) 
  
  signum Zero = Zero
  signum (Succ a) = Succ Zero
  signum (Pred a) = Pred Zero

  fromInteger i 
            | i == 0 = Zero
            | i > 0 = Succ (fromInteger(i - 1))
            | i < 0 = Pred (fromInteger(i - 1))

instance Eq WeirdPeanoNumber where
    a == b = (wpnToInt a) == (wpnToInt b) 
    
instance Ord WeirdPeanoNumber where
  compare a b
         | (wpnToInt a) == (wpnToInt b) =  EQ
         | (wpnToInt a) <= (wpnToInt b) =  LT 
         | otherwise =  GT 
