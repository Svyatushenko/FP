module ITMOPrelude.Tree where

import Prelude (Show,Read,error)

data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Show,Read)

-- Создание пустого дерева
createTree :: Tree a
createTree = Leaf

--Добавление элемента в вершину дерева
addNode :: Tree a -> a -> Tree a
addNode Leaf x 		= Node x Leaf Leaf
addNode (Node n l r) x 	= Node x l r

--Добавление элемента в качестве самого левого
addLeft :: Tree a -> a -> Tree a
addLeft Leaf x 		= Node x Leaf Leaf
addLeft (Node n l r) x 	= addLeft l x

--Добавление элемента в качестве самого правого
addRight :: Tree a -> a -> Tree a
addRight Leaf x 	= Node x Leaf Leaf
addRight (Node n l r) x = addRight r x

--Левый поворот
leftRot :: Tree a -> Tree a
leftRot Leaf 			  = Leaf
leftRot (Node n l Leaf)		  = Node n l Leaf
leftRot (Node n l (Node r rl ll)) = Node r (Node n l rl) ll

--Правый поворот
rightRot :: Tree a -> Tree a
rightRot Leaf 			   = Leaf
rightRot (Node n Leaf r) 	   = Node n Leaf r
rightRot (Node n (Node l ll lr) r) = Node l ll (Node n lr r)

-- Аналог функции map
tmap :: (a -> b) -> Tree a -> Tree b
tmap f Leaf 	    = Leaf
tmap f (Node a l r) = Node (f a) (tmap f l) (tmap f r)

-- Аналог функции foldr
foldr :: (a -> b -> b) -> b -> Tree a -> b
foldr f z Leaf = z
foldr f z (Node a l r) = foldr f (f a (foldr f z r)) l