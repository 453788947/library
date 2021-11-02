{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Function (on)
import Generic.Random (genericArbitrary, genericArbitraryU, (%))
import Lib
import Test.HUnit
import Test.QuickCheck

tests =
  TestList
    [ TestList [],
      task1Test,
      task2Test,
      TestList [],
      task4Test,
      task5Test,
      task6Test,
      task7Test,
      task8Test,
      task9Test,
      task10Test
    ]

main = runTestTT tests

qcArgs = stdArgs {chatty = False}

quickTestToTestCase descr test = TestCase $ do
  result <- test
  let message = "ожидается, что " ++ descr ++ ", но\n" ++ output result
  if isSuccess result
    then pure ()
    else assertBool message False

testApplicativeFunctor ::
  forall f.
  ( forall a. Show a => Show (f a),
    forall a. Eq a => Eq (f a)
  ) =>
  -- | генератор для структуры
  (forall a. Arbitrary a => Gen (f a)) ->
  -- | функция, которая усекает структуру до конечного
  --  размера; для заведомо конечной структуры можно
  --  использовать 'id'
  (forall a. f a -> f a) ->
  -- | fmap
  (forall a b. (a -> b) -> f a -> f b) ->
  -- | pure
  (forall a. a -> f a) ->
  -- | (<*>)
  (forall a b. f (a -> b) -> f a -> f b) ->
  -- | аппликативный функтор ли это
  Property
testApplicativeFunctor gen cutoff fmap pure (<*>) =
  property $
    counterexample
      "первый закон аппликативного функтора"
      (forAll (gen :: Gen (f Int)) (\x -> pure id <*> x === x))
      .&. counterexample
        "второй закон аппликативного функтора"
        ( forAll (arbitrary :: Gen (Fun Int String, Int)) $ \(Fun _ f, i) ->
            cutoff (pure f <*> pure i) === cutoff (pure (f i))
        )
      .&. counterexample
        "третий закон аппликативного функтора"
        ( forAll (gen :: Gen (f (Fun Int String))) $ \uPre ->
            let u = fmap (\(Fun _ f) -> f) uPre
             in forAll (arbitrary :: Gen Int) $ \i ->
                  u <*> pure i === pure ($ i) <*> u
        )
      .&. counterexample
        "четвёртый закон аппликативного функтора"
        ( forAll (gen :: Gen (f (Fun Int String))) $ \uPre ->
            let u = fmap (\(Fun _ f) -> f) uPre
             in forAll (gen :: Gen (f (Fun Char Int))) $ \vPre ->
                  let v = fmap (\(Fun _ f) -> f) vPre
                   in forAll (gen :: Gen (f Char)) $ \x ->
                        ((pure (.) <*> u) <*> v) <*> x === u <*> (v <*> x)
        )
      .&. counterexample
        "аппликативный функтор согласован с функтором"
        ( forAll (arbitrary :: Gen (Fun Int Char)) $ \(Fun _ g) ->
            forAll (gen :: Gen (f Int)) $ \x ->
              fmap g x === pure g <*> x
        )

-- Task 1 ---------------------------------------------------------------------

task1Test =
  TestList $
    [ quickTestToTestCase "исключение к полугруппе" $
        quickCheckWithResult qcArgs rule1,
      quickTestToTestCase "операция полугруппы" $
        quickCheckWithResult qcArgs rule2,
      quickTestToTestCase "контрпример к моноиду" $
        quickCheckWithResult qcArgs rule3,
      quickTestToTestCase "единица моноида" $
        quickCheckWithResult qcArgs rule4
    ]
  where
    -- |Проверяем, правда ли, что на данных числах gcd не ассоциативен.
    rule1 :: () -> Bool
    rule1 _ = case task1 of
      Left (a', b', c') ->
        ((a' `gcd` b') `gcd` c') /= (a' `gcd` (b' `gcd` c'))
      _ -> True
    -- |Проверяем, выполняется ли ассоциативность операции.
    rule2 :: Int -> Int -> Int -> Bool
    rule2 a b c =
      let (ga, gb, gc) = (Gcd a, Gcd b, Gcd c)
       in case task1 of
            Right (op, r) -> ((ga `op` gb) `op` gc) == (ga `op` (gb `op` gc))
            _ -> True
    -- |Проверяем, есть ли какое-то такое число, что gcd с ним контрпримера
    -- равно контрпримеру.
    rule3 :: Int -> Bool
    rule3 a = case task1 of
      Right (op, Left a') ->
        Gcd a' `op` Gcd a /= Gcd a' || Gcd a `op` Gcd a' /= Gcd a'
      _ -> True
    -- |Убеждаемся, что единица действительно является единицей.
    rule4 :: Int -> Property -- более понятные ошибки, хотя писать посложнее
    rule4 a = case task1 of
      Right (op, Right ga') ->
        ga' `op` Gcd a === Gcd a .&&. Gcd a `op` ga' === Gcd a
      _ -> property True
      -- см. `property`, `===`, `.&&.` в
      -- https://hackage.haskell.org/package/QuickCheck-2.14.2/docs/Test-QuickCheck.html

-- Task 2 ---------------------------------------------------------------------

-- Ваши тесты идут сюда:
task2Test =
  TestList
    []

-- Task 3 ---------------------------------------------------------------------

-- Проверяется вместе с предыдущим заданием.

-- Task 4 ---------------------------------------------------------------------

instance Arbitrary e => Arbitrary (HalfPair e a) where
  arbitrary = genericArbitraryU

task4Test =
  TestList $
    [ quickTestToTestCase "аппликативный функтор HalfPair корректный" $
        quickCheckWithResult qcArgs $
          let (fmap, pure, (<*>)) = task4
           in testApplicativeFunctor
                arbitrary
                (id :: forall a. HalfPair String a -> HalfPair String a)
                fmap
                pure
                (<*>)
    ]

-- Task 5 ---------------------------------------------------------------------

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = genericArbitraryU

task5Test =
  TestList $
    [ quickTestToTestCase "аппликативный функтор Tree корректный" $
        quickCheckWithResult qcArgs $
          let (fmap, pure, (<*>)) = task5
           in testApplicativeFunctor
                arbitrary
                (takeNLevels 5)
                fmap
                pure
                (<*>)
    ]
  where
    takeNLevels :: Int -> Tree a -> Tree a
    takeNLevels 0 _ = Leaf
    takeNLevels n Leaf = Leaf
    takeNLevels n (Node v l r) = (Node v `on` takeNLevels (n - 1)) l r

-- Task 6 ---------------------------------------------------------------------

instance Show a => Show (DoubleReader Int Bool a) where
  show (DoubleReader f) =
    concat $ do
      i <- ints
      b <- bools
      pure $
        "{ " ++ show i ++ ", " ++ show b
          ++ " -> "
          ++ show (f i b)
          ++ "}"
    where
      ints = [-3 .. 3]
      bools = [True, False]

instance Arbitrary a => Arbitrary (DoubleReader Int Bool a) where
  arbitrary =
    (DoubleReader . applyFun2)
      <$> (arbitrary :: Gen (Fun (Int, Bool) a))

instance Eq a => Eq (DoubleReader Int Bool a) where
  (DoubleReader f) == (DoubleReader g) =
    and $ do
      i <- ints
      b <- bools
      pure $ f i b == g i b
    where
      ints = [-3 .. 3]
      bools = [True, False]

task6Test =
  TestList $
    [ quickTestToTestCase "аппликативный функтор DoubleReader корректный" $
        quickCheckWithResult qcArgs $
          let (fmap, pure, (<*>)) = task6
           in testApplicativeFunctor
                arbitrary
                (id :: forall a. DoubleReader Int Bool a -> DoubleReader Int Bool a)
                fmap
                pure
                (<*>)
    ]

-- Task 7 ---------------------------------------------------------------------

instance Arbitrary a => Arbitrary (Point a) where
  arbitrary = genericArbitraryU

task7Test =
  TestList $
    [ quickTestToTestCase "аппликативный функтор Point корректный" $
        quickCheckWithResult qcArgs $
          let (fmap, pure, (<*>)) = task7
           in testApplicativeFunctor
                arbitrary
                (id :: forall a. Point a -> Point a)
                fmap
                pure
                (<*>)
    ]

-- Task 8 ---------------------------------------------------------------------

task8Test =
  TestList $
    [ TestCase $
        assertEqual
          "список Just, "
          (Just [3, 4])
          (task8 [Just 3, Just 4]),
      TestCase $
        assertEqual
          "список с Nothing, "
          Nothing
          (task8 [Just 3, Nothing, Just 5]),
      TestCase $
        assertEqual
          "список списков, "
          [[1, 4], [1, 5], [2, 4], [2, 5], [3, 4], [3, 5]]
          (task8 [[1, 2, 3], [4, 5]]),
      TestCase $
        assertEqual
          "список функций, "
          [8, 10, 25]
          (task8 [\x -> x + 3, \x -> x * 2, \x -> x ^ 2] 5),
      TestCase $
        assertEqual
          "бесконечный список, "
          [8, 10, 25]
          (take 3 $ task8 ([\x -> x + 3, \x -> x * 2, \x -> x ^ 2] ++ undefined) 5)
    ]

-- Task 9 ---------------------------------------------------------------------

task9Test =
  TestList $
    [ TestCase $
        assertEqual
          "список Just, "
          (Just [3, 4])
          (task9 [Just 3, Just 4]),
      TestCase $
        assertEqual
          "список с Nothing, "
          Nothing
          (task9 [Just 3, Nothing, Just 5]),
      TestCase $
        assertEqual
          "список списков, "
          [[1, 4], [1, 5], [2, 4], [2, 5], [3, 4], [3, 5]]
          (task9 [[1, 2, 3], [4, 5]]),
      TestCase $
        assertEqual
          "список функций, "
          [8, 10, 25]
          (task9 [\x -> x + 3, \x -> x * 2, \x -> x ^ 2] 5),
      TestCase $
        assertBool
          "неправильный аппликативный функтор"
          task9'
    ]

-- Task 10 --------------------------------------------------------------------

instance Arbitrary a => Arbitrary (Lam a) where
  arbitrary = genericArbitraryU

task10Test =
  TestList $
    [ quickTestToTestCase "аппликативный функтор Lam корректный" $
        quickCheckWithResult qcArgs $
          let (fmap, pure, (<*>)) = task10
           in testApplicativeFunctor
                arbitrary
                (id :: forall a. Lam a -> Lam a)
                fmap
                pure
                (<*>)
    ]
