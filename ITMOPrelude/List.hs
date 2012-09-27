{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.List where

import Prelude (Show,Read,error)
import ITMOPrelude.Primitive

---------------------------------------------
-- Что надо делать?
--
-- Все undefined превратить в требуемые термы.
-- Звёздочкой (*) отмечены места, в которых может потребоваться думать.

---------------------------------------------
-- Определение

data List a = Nil |  Cons a (List a) deriving (Show,Read)

---------------------------------------------
-- Операции

-- Длина списка
length :: List a -> Nat
length Nil = Zero
length (Cons a n) = Succ (length n)

-- Склеить два списка за O(length a)
(++) :: List a -> List a -> List a
Nil ++ b = b
(Cons a n) ++ b = Cons a (n ++ b)

-- Список без первого элемента
tail :: List a -> List a
tail Nil = Nil
tail (Cons a n) = n

-- Список без последнего элемента
init :: List a -> List a
init Nil = Nil
init (Cons a Nil) = Nil
init (Cons a n) = Cons a (init n)

-- Первый элемент
head :: List a -> a
head (Cons a n) = a

-- Последний элемент
last :: List a -> a
last (Cons a Nil) = a
last (Cons a n) = last n

-- n первых элементов списка
take :: Nat -> List a -> List a
take Zero a 		 = Nil
take (Succ n) (Cons a t) = Cons a (take n t)

-- Список без n первых элементов
drop :: Nat -> List a -> List a
drop Zero a 		 = a
drop (Succ n) (Cons a t) = drop n t

-- Оставить в списке только элементы удовлетворяющие p
filter :: (a -> Bool) -> List a -> List a
filter p Nil 	    = Nil
filter p (Cons a n) = if' (p a) (Cons a (filter  p n)) (filter  p n)

-- Обобщённая версия. Вместо "выбросить/оставить" p
-- говорит "выбросить/оставить b".
gfilter :: (a -> Maybe b) -> List a -> List b
gfilter p Nil = Nil
gfilter p (Cons a n) = case (p a) of
			(Just b) -> Cons b (gfilter p n)
			Nothing -> gfilter p n

-- Копировать из списка в результат до первого нарушения предиката
-- takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
takeWhile :: (a -> Bool) -> List a -> List a
takeWhile p Nil = Nil
takeWhile p (Cons a n) = if' (p a) (Cons a (takeWhile p n)) Nil

-- Не копировать из списка в результат до первого нарушения предиката,
-- после чего скопировать все элементы, включая первый нарушивший
-- dropWhile (< 3) [1,2,3,4,1,2,3,4] == [3,4,1,2,3,4]
dropWhile :: (a -> Bool) -> List a -> List a
dropWhile p Nil = Nil
dropWhile p (Cons a n) = if' (p a) (dropWhile p n) (Cons a n)

-- Разбить список по предикату на (takeWhile p xs, dropWhile p xs),
-- но эффективнее
span :: (a -> Bool) -> List a -> Pair (List a) (List a)
span p Nil 	   = Pair Nil Nil
span p (Cons a n)  = if' (p a) (Pair (Cons a (fst (span p n))) (snd (span p n))) (Pair Nil (Cons a n))

-- Разбить список по предикату на (takeWhile (not . p) xs, dropWhile (not . p) xs),
-- но эффективнее
break :: (a -> Bool) -> List a -> Pair (List a) (List a)
break p a = span (not . p) a

-- n-ый элемент списка (считая с нуля)
(!!) :: List a -> Nat -> a
Nil !! n = error "!!: empty list"
(Cons a t) !! Zero 	= a
(Cons a t) !! (Succ n)  = t !! n

-- Список задом на перёд
reverse :: List a -> List a
reverse Nil 	   = Nil
reverse (Cons a n) = (reverse n) ++ (Cons a Nil)

-- (*) Все подсписки данного списка
subsequences :: List a -> List (List a)
subsequences Nil 	 = Cons Nil Nil
subsequences (Cons a n)  = map (Cons a) (subsequences n) ++ subsequences n


-- (*) Все перестановки элементов данного списка
permutations :: List a -> List (List a)
permutations = undefined

-- (*) Если можете. Все перестановки элементов данного списка
-- другим способом
permutations' :: List a -> List (List a)
permutations' = undefined

-- Повторяет элемент бесконечное число раз
repeat :: a -> List a
repeat a = Cons a (repeat a)

-- Левая свёртка
-- порождает такое дерево вычислений:
--         f
--        / \
--       f   ...
--      / \
--     f   l!!2
--    / \
--   f   l!!1
--  / \
-- z  l!!0
foldl :: (a -> b -> a) -> a -> List b -> a
foldl f z Nil = z
foldl f z (Cons a n) = foldl f (f z a) n

-- Тот же foldl, но в списке оказываются все промежуточные результаты
-- last (scanl f z xs) == foldl f z xs
scanl :: (a -> b -> a) -> a -> List b -> List a
scanl f z Nil = Cons z Nil
scanl f z (Cons a n) = Cons b (scanl f b n) where b = f z a

-- Правая свёртка
-- порождает такое дерево вычислений:
--    f
--   /  \
-- l!!0  f
--     /  \
--   l!!1  f
--       /  \
--    l!!2  ...
--           \
--            z
--            
foldr :: (a -> b -> b) -> b -> List a -> b
foldr f z Nil = z
foldr f z (Cons a n) = f a (foldr f z n) 

-- Аналогично
--  head (scanr f z xs) == foldr f z xs.
scanr :: (a -> b -> b) -> b -> List a -> List b
scanr f z Nil = Cons z Nil
scanr f z (Cons a n) = Cons (f a b) bn where (Cons b bn) = scanr f z n

-- Должно завершаться за конечное время
finiteTimeTest = take (Succ $ Succ $ Succ $ Succ Zero) $ foldr (Cons) Nil $ repeat Zero

-- Применяет f к каждому элементу списка
map :: (a -> b) -> List a -> List b
map f (Cons a Nil) = Cons (f a) Nil
map f (Cons a n) = Cons (f a) (map f n) 

-- Склеивает список списков в список
concat :: List (List a) -> List a
concat Nil = Nil
concat (Cons a n) = a ++ (concat n)

-- Эквивалент (concat . map), но эффективнее
concatMap :: (a -> List b) -> List a -> List b
concatMap f Nil = Nil
concatMap f (Cons a n) = (f a) ++ (concatMap f n)

-- Сплющить два списка в список пар длинны min (length a, length b)
zip :: List a -> List b -> List (Pair a b)
zip Nil a = Nil
zip a Nil = Nil
zip (Cons a n) (Cons b m) = Cons (Pair a b) (zip n m)

-- Аналогично, но плющить при помощи функции, а не конструктором Pair
zipWith :: (a -> b -> c) -> List a -> List b -> List c
zipWith f a Nil = Nil
zipWith f Nil a = Nil
zipWith f (Cons a n) (Cons b m) = Cons (f a b) (zipWith f n m)