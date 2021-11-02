{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Lib
  ( Tree (..),
    HalfPair (..),
    DoubleReader (..),
    Point (..),
    Lam (..),
    Gcd (..),
    IntOp (..),
    task1,
    task2,
    task4,
    task5,
    task6,
    task7,
    task8,
    task9,
    task9',
    task10,
  )
where

import Control.Monad (ap)

{-
> ("xyz", 3) <* ("zy", 4)
> ($ 3) <$> [(+2), (*3), (`div` 2)]
> ($) <$> [(+2), (*3), (`div` 2)] <*> [3, 10]
> [(++"x"),(show . length)] <*> ["abc", "def", "ghi"]
> getZipList $ (,,) <$> ZipList "cat" <*> ZipList "dog" <*> ZipList "mouse"

Если окажется, что какое-то поведение здесь непонятно, то найдите исходники
соответствующих операторов либо экземпляров Functor и Applicative и либо
внимательно прочитайте, либо попробуйте воссоздать.

Загрузите в интерпретаторе файл с тестами.
> quickCheck False
> quickCheck True
> quickCheck $ \x -> x + x /= 6
> quickCheck $ \x -> x <= (0 :: Int) || x + x > x
> quickCheck $ \(Large x) -> x <= (0 :: Int) || x + x > x
> sample (arbitrary :: Gen Int)
> sample (arbitrary :: Gen (Large Int))
> sample (arbitrary :: Gen String)
> sample (arbitrary :: Gen ASCIIString)
> :i arbitrary
> :i quickCheck
> :i Test.QuickCheck.Testable
-}

-- Нужно, чтобы QuickCheck автоматически генерировал экземпляры 'Arbitrary'.
import GHC.Generics (Generic)

{- 1. Задаёт ли операция поиска наибольшего общего делителя моноид? Полугруппу?
Если да, реализуйте соответствующие экземпляры для обёртки `Gcd`. Если нет,
предоставьте контрпример. Контрпример для полугруппы -- тройка элементов, на
которых нарушается ассоциативность (то есть `a <> (b <> c) != (a <> b) <> c`),
а для моноида -- такой элемент `a`, что не существует такого элемента `e`, что
`e <> a == a` и `a <> e == a`. -}

newtype Gcd a = Gcd a deriving (Show, Eq)

instance (Integral a, Eq a) => Semigroup (Gcd a) where
    (Gcd x) <> (Gcd y) = if y == 0 then Gcd x else Gcd y <> Gcd (x `mod` y)

instance Integral a => Monoid (Gcd a) where
    mempty  = Gcd 1
    mappend x y = x <> y
    mconcat = foldr mappend mempty

{- Left (a, b, c), если gcd a (gcd b c) /= gcd (gcd a b) c для каких-то a, b и c.
В противном случае Right (<>, ???), где ??? -- это
Right mempty, если определён моноид; Left a, если для какого-то `a` нет левой
или правой единицы. -}
task1 ::
  Integral a =>
  Either (a, a, a) (Gcd a -> Gcd a -> Gcd a, Either a (Gcd a))
task1 = Left (1, 2, 3)

{- 2. Задаёт ли моноид операция
`\x y -> ((x * y) `div` (if x /= 0 then x else 1) * x` на числах типа
`Int`? А полугруппу? -}

newtype IntOp = IntOp Int deriving (Show, Eq)

task2 :: Either (Int, Int, Int) (IntOp -> IntOp -> IntOp, Either Int IntOp)
task2 = undefined

{- 3. Используя библиотеку QuickCheck, задайте в файле с тестами правила,
проверяющие предыдущее задание. В качестве примера можно взять правила,
определённые в тестовом модуле для первой задачи.

Не нужно разбирать тщательно task2, как это сделано в тестах для task1: я-то не
знаю, какой ответ даст студент, а вы уже заведомо в курсе, какая часть тестов не
будет участвовать в проверке.

Если тест не находит ошибку в прошлом задании, а она там есть, то оба не
засчитываются. -}

{- 4. Сделайте экземпляром Applicative следующую структуру данных в
предположении, что первая компонента моноид: -}

data HalfPair e a = HalfPair {runHalfPair :: e}
  deriving (Eq, Show, Generic)

instance Functor (HalfPair e) where
    fmap f = undefined

instance Applicative (HalfPair e) where
    pure = undefined
    (<*>) = undefined

task4 = (fmap, pure, (<*>))

{- 5. Сделайте экземпляром Applicative следующую структуру данных: -}

data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving (Eq, Show, Generic)

instance Functor Tree where
    fmap f Leaf = Leaf
    fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)

instance Applicative Tree where
    pure x = let t = Node x t t in t
    (<*>) Leaf _ = Leaf
    (<*>) _ Leaf = Leaf
    (<*>) (Node f lf rf) (Node a la ra) = Node (f a) (lf <*> la) (rf <*> ra)

task5 = (fmap, pure, (<*>))

{- 6. Сделайте экземпляром Applicative следующую структуру данных: -}

newtype DoubleReader e e' a = DoubleReader {runDoubleReader :: e -> e' -> a}

instance Functor (DoubleReader a b) where
    fmap = undefined

instance Applicative (DoubleReader a b) where
    pure = undefined
    x <*> y = undefined

task6 = (fmap, pure, (<*>))

{- 7. Сделайте экземпляром Applicative следующую структуру данных: -}

data Point a = Point a a deriving (Show, Eq, Generic)

instance Functor Point where
    fmap f (Point x y) = Point (f x) (f y)

instance Applicative Point where
    pure x = Point x x
    x <*> y = undefined

task7 = (fmap, pure, (<*>))

{- 8. Реализуйте функцию, которая принимает на вход список значений в
каком-то аппликативном контексте, последовательно "выполняет" каждое
значение и возвращает список результатов исполнения каждого значения в
данном контексте. В тестах есть примеры. -}

task8 :: Applicative f => [f a] -> f [a]
-- task8 (x:xs) = (:) <$> x <*> (pure xs)
task8 = undefined

{- 9. Вероятно, функция из предыдущего задания выполняется по схеме
```
task5 [a, b, c] ==
a
{
  b
  {
    c
  }
}
```
Так проще всего реализовать. Если нет, реализуйте функцию по такой схеме.
Если да, напишите теперь такую функцию, что
```
task5 [a, b, c] ==
{
  {
    a
  }
  b
}
c
```
Согласно законам аппликативных функторов, это одно и то же. Задайте такой
аппликативный функтор, что на нём `task5` и `task6` будут давать разный
результат. -}

task9 :: Applicative f => [f a] -> f [a]
task9 = undefined

task9' = undefined -- task5 lst /= task6 lst
  where
    -- Список, на котором task5 и task6 дают разные результаты.
    lst = undefined

{- 10. Сделайте аппликативным функтором следующую структуру данных: -}

data Lam a = Var a | App (Lam a) (Lam a) | Lam (Lam (Maybe a))
  deriving (Eq, Show, Generic)

task10 = undefined -- (fmap, pure, (<*>))
