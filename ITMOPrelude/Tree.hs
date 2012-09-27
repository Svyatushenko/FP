module ITMOPrelude.Tree where

import Prelude (Show,Read,error)

data Tree a = Nil | Node a (Tree a) (Tree a) deriving (Show,Read)

-- Создание пустого дерева
createTree :: Tree a
createTree = Nil

--Добавление элемента в вершину дерева
addNode :: Tree a -> a -> Tree a
addNode Nil x 		= Node x Nil Nil
addNode (Node n l r) x 	= Node x l r

--Добавление элемента в качестве самого левого
addLeft :: Tree a -> a -> Tree a
addLeft Nil x 		= Node x Nil Nil
addLeft (Node n l r) x 	= addLeft l x

--Добавление элемента в качестве самого правого
addRight :: Tree a -> a -> Tree a
addRight Nil x 		= Node x Nil Nil
addRight (Node n l r) x = addRight r x

--Левый поворот
leftRot :: Tree a -> Tree a
leftRot Nil 			  = Nil
leftRot (Node n l Nil) 		  = Node n l Nil
leftRot (Node n l (Node r rl ll)) = Node r (Node n l rl) ll

--Правый поворот
rightRot :: Tree a -> Tree a
rightRot Nil 			   = Nil
rightRot (Node n Nil r) 	   = Node n Nil r
rightRot (Node n (Node l ll lr) r) = Node l ll (Node n lr r)

-- Аналог функции map
map :: (a -> b) -> Tree a -> Tree b
map f Nil 	   = Nil
map f (Node a l r) = Node (f a) (map f l) (map f r)

-- Аналог функции foldr
foldr :: (a -> b -> b) -> b -> Tree a -> b
foldr f z Nil = z
foldr f z (Node a l r) = foldr f (f a (foldr f z r)) l