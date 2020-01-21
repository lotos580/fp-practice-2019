module Task1_1 where

{-
  Задание 1.1
  Необходимо реализовать все операции, описанные в данном файле
-}

import Todo(todo)

data Term = IntConstant{ intValue :: Int }           -- числовая константа
            | Variable{ varName :: String }          -- переменная
            | BinaryTerm{ lhv :: Term, rhv :: Term } -- бинарная операция
            deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) IntConstant{intValue=l} IntConstant{intValue=r} = IntConstant{intValue=( l + r )}
(|+|) BinaryTerm{lhv=a, rhv=b} IntConstant{intValue=c} = (|+|) ((|+|) a b) IntConstant{intValue=c}
(|+|) IntConstant{intValue=a} BinaryTerm{lhv=b, rhv=c} = (|+|) IntConstant{intValue=a} ((|+|) b c)
(|+|) BinaryTerm{lhv=a, rhv=b} BinaryTerm{lhv=c, rhv=d} = (|+|) ((|+|) a b) ((|+|) c d)  

(|-|) :: Term -> Term -> Term 
(|-|) IntConstant{intValue=l} IntConstant{intValue=r} = IntConstant{intValue=( l - r )}
(|-|) BinaryTerm{lhv=a, rhv=b} IntConstant{intValue=c} = (|-|) ((|-|) a b) IntConstant{intValue=c}
(|-|) IntConstant{intValue=a} BinaryTerm{lhv=b, rhv=c} = (|-|) IntConstant{intValue=a} ((|+|) b c)
(|-|) BinaryTerm{lhv=a, rhv=b} BinaryTerm{lhv=c, rhv=d} = (|-|) ((|-|) a b) ((|+|) c d)  

(|*|) :: Term -> Term -> Term
(|*|) IntConstant{intValue=a} IntConstant{intValue=b} = IntConstant{intValue=(a*b)}
(|*|) BinaryTerm{lhv=a, rhv=b} IntConstant{intValue=c} = (|*|) ((|*|) a b) IntConstant{intValue=c}
(|*|) IntConstant{intValue=a} BinaryTerm{lhv=b, rhv=c} = (|*|) IntConstant{intValue=a} ((|*|) b c)
(|*|) BinaryTerm{lhv=a, rhv=b} BinaryTerm{lhv=c, rhv=d} = (|*|) ((|*|) a b) ((|*|) c d)  
--(|*|) Variable{} IntConstant{intValue=b} = IntConstant{intValue=(1*b)}
--(|*|) IntConstant{intValue=a} Variable{} = IntConstant{intValue=(a*1)}
--(|*|) Variable{} Variable{} = IntConstant{intValue=(1*1)}
--(|*|) BinaryTerm{lhv=a, rhv=b} Variable{} = (|*|) ((|*|) a b) IntConstant{intValue=1}
--(|*|) Variable{} BinaryTerm{lhv=c, rhv=d} = (|*|) IntConstant{intValue=1} ((|*|) c d)


-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar oldname replacement expression = case expression of  
    Variable{} -> if varName expression == oldname then expression{varName=( varName replacement )} else expression 
    IntConstant{} -> expression
    BinaryTerm{} -> BinaryTerm {
                        lhv=( replaceVar oldname replacement ( lhv expression ) ), 
                        rhv=( replaceVar oldname replacement ( rhv expression ) ) } 

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate expression = case expression of 
                              Variable{} -> IntConstant{intValue=0} 
                              IntConstant{} -> IntConstant{intValue= (intValue expression)}  
                              BinaryTerm{lhv=IntConstant{}, rhv=IntConstant{}} -> (|*|) (lhv expression) (rhv expression)
                              otherwise -> IntConstant{intValue=0}  