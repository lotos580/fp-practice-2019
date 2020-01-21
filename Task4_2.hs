module Task4_2 where

{-
  Задание 4.1
  Реализация монады над множеством из четырёх элементов.
  Реализуйте все требуемые классы типов и объясните, почему они реализованы именно так.
-}

data FourOf a = FourOf a a a a deriving(Show,Eq)

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FourOf`
-- таким образом, что
-- do { x <- FourOf 1 2 3 4; y <- FourOf 4 6 7 8; return $ x + y } === FourOf 5 8 10 12

instance Functor FourOf where 
    fmap f (FourOf a1 a2 a3 a4) = FourOf (f a1 ) (f a2 ) (f a3 ) (f a4 )
    -- при применении функции к FourOf будем получать FourOf где данная функция применена к каждому из элементов
    -- поименовка элементов используется чтобы различить элементы и сохранить их на прежних местах

instance Applicative FourOf where
    pure a = FourOf a a a a
    FourOf f g h t <*> FourOf a1 a2 a3 a4 = FourOf ( f a1 ) ( g a2 ) ( h a3 ) ( t a4 )
    -- определяем применение обёрнутых в FourOf функций
    -- поименовка элементов и функций используется чтобы различить элементы и сохранить порядки


instance Monad FourOf where 
    -- return не описан т.к. присутствует pure у Applicative FourOf
    (FourOf a1 a2 a3 a4) >>= f = FourOf ( getF (f a1) ) ( getS (f a2) ) ( getT (f a3) ) ( getR (f a4) ) 
                                  where getF ( FourOf a1 _ _ _ ) = a1
                                        getS ( FourOf _ a2 _ _ ) = a2
                                        getT ( FourOf _ _ a3 _ ) = a3
                                        getR ( FourOf _ _ _ a4 ) = a4
    -- при применении bind-оператора возвращаем FourOf с поэлементным применением использованной функции
    -- вспомогательные функции необоходимы только для того чтобы возвратить элемент с правильной позиции