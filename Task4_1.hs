module Task4_1 where

{-
  Задание 4.1
  Реализация монады над функцией.
  Реализуйте все требуемые классы типов и объясните, почему они реализованы именно так.
-}

-- Монада над функцией. В качестве входного значения `fun` может быть что угодно
-- Собственно, почему бы не `String`?
data FunMonad a = FunMonad { fun :: String -> a }


-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FunMonad` 

instance Functor FunMonad where
    fmap f (FunMonad x) = FunMonad (\a -> f (x a)) 
    -- определяем перепаковку функции fun в данной FunMonad

instance Applicative FunMonad where
    pure a = FunMonad ( \x -> a )
    (FunMonad f) <*> (FunMonad g) = FunMonad ( \x -> (f x) (g x) )
    -- определяем комбинацию функций из FunMonad с перепаковкой обратно

instance Monad FunMonad where
    return a = FunMonad (\x -> a)
    FunMonad a >>= f = FunMonad (\x -> fun ( f (a x) ) x)
    -- порождаем новую FunMonad на основе комбинирования имевшейся fun с новой функцией