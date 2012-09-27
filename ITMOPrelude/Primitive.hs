module ITMOPrelude.Primitive where

import Prelude (Show,Read,error)

undefined = undefined
-------------------------------------------
-- Примитивные типы

-- Тип с единственным элементом
data Unit = Unit deriving (Show,Read)

-- Пара, произведение
data Pair a b = Pair { fst :: a, snd :: b } deriving (Show,Read)

-- Вариант, копроизведение
data Either a b = Left a | Right b deriving (Show,Read)

-- Частый частный случай, изоморфно Either Unit a
data Maybe a = Nothing | Just a deriving (Show,Read)

-- Частый частный случай, изоморфно Either Unit Unit
data Bool = False | True deriving (Show,Read)

-- Следует отметить, что встроенный if с этим Bool использовать нельзя,
-- зато case всегда работает.

-- Ну или можно реализовать свой if
if' True a b = a
if' False a b = b

-- Трихотомия. Замечательный тип, показывающий результат сравнения
data Tri = LT | EQ | GT deriving (Show,Read)

-------------------------------------------
-- Булевы значения

-- Логическое "НЕ"
not :: Bool -> Bool
not True = False
not False = True

infixr 3 &&
-- Логическое "И"
(&&) :: Bool -> Bool -> Bool
True  && x = x
False && _ = False

infixr 2 ||
-- Логическое "ИЛИ"
(||) :: Bool -> Bool -> Bool
True  || _ = True
False || x = x

-------------------------------------------
-- Натуральные числа

data Nat = Zero | Succ Nat deriving (Show,Read)

natZero = Zero     -- 0
natOne = Succ Zero -- 1

-- Сравнивает два натуральных числа
natCmp :: Nat -> Nat -> Tri
natCmp n m = if' (natEq n m) (EQ) (if' (natLt n m) (LT) (GT))

-- n совпадает с m 
natEq :: Nat -> Nat -> Bool
natEq Zero     Zero     = True
natEq Zero     (Succ _) = False
natEq (Succ _) Zero     = False
natEq (Succ n) (Succ m) = natEq n m

-- n меньше m
natLt :: Nat -> Nat -> Bool
natLt Zero     Zero     = False
natLt Zero     (Succ m) = True
natLt (Succ n) Zero     = False
natLt (Succ n) (Succ m) = natLt n m

-- n больше m
natGt :: Nat -> Nat -> Bool
natGt Zero     Zero     = False
natGt Zero     (Succ m) = False
natGt (Succ n) Zero     = True
natGt (Succ n) (Succ m) = natGt n m

infixl 6 +.
-- Сложение для натуральных чисел
(+.) :: Nat -> Nat -> Nat
Zero     +. m = m
(Succ n) +. m = Succ (n +. m)

infixl 6 -.
-- Вычитание для натуральных чисел
(-.) :: Nat -> Nat -> Nat
Zero	 -. _ = Zero
(Succ n) -. Zero = (Succ n)
(Succ n) -. (Succ m) = n -. m

infixl 7 *.
-- Умножение для натуральных чисел
(*.) :: Nat -> Nat -> Nat
Zero     *. m = Zero
(Succ n) *. m = m +. (n *. m)

-- Целое и остаток от деления n на m
natDivMod :: Nat -> Nat -> Pair Nat Nat
natDivMod n Zero = error "Division by zero"
natDivMod n m = if' (natLt n m) (Pair Zero n) (Pair (Succ $ natDiv (n -. m) m) (natMod (n -. m) m))

natDiv n = fst . natDivMod n -- Целое
natMod n = snd . natDivMod n -- Остаток

-- Поиск GCD алгоритмом Евклида (должен занимать 2 (вычислителельная часть) + 1 (тип) строчки)
gcd :: Nat -> Nat -> Nat
gcd n Zero = n
gcd n m    = gcd m (natMod n m)

-------------------------------------------
-- Целые числа

-- Требуется, чтобы представление каждого числа было единственным
data Int = Pos Nat | Neg Nat deriving (Show,Read)

intZero   = Pos Zero   -- 0
intOne    = Pos (Succ Zero)     -- 1
intNegOne = Neg Zero -- -1

-- n -> - n
intNeg :: Int -> Int
intNeg (Pos (Succ n)) = Neg n
intNeg (Neg n)        = Pos (Succ n)
intNeg (Pos Zero)     = Pos Zero


-- Дальше также как для натуральных
intCmp :: Int -> Int -> Tri
intCmp n m = if' (intEq n m) (EQ) (if' (intLt n m) (LT) (GT))


intEq :: Int -> Int -> Bool
intEq (Neg _) (Pos _) = False
intEq (Pos _) (Neg _) = False
intEq (Neg n) (Neg m) = natEq m n
intEq (Pos n) (Pos m) = natEq n m

intLt :: Int -> Int -> Bool
intLt (Neg _) (Pos _) = True
intLt (Pos _) (Neg _) = False
intLt (Neg n) (Neg m) = natLt m n
intLt (Pos n) (Pos m) = natLt n m

intGt :: Int -> Int -> Bool
intGt (Neg _) (Pos _) = False
intGt (Pos _) (Neg _) = True
intGt (Neg n) (Neg m) = natGt m n
intGt (Pos n) (Pos m) = natGt n m

infixl 6 .+., .-.
-- У меня это единственный страшный терм во всём файле
(.+.) :: Int -> Int -> Int
(Pos Zero) .+. n 		   = n
n .+. (Pos Zero) 		   = n
(Pos (Succ n)) .+. (Neg Zero) 	   = Pos n
(Pos (Succ n)) .+. (Neg (Succ m))  = Pos n .+. Neg m
(Pos n) .+. (Pos m) 		   = Pos (n +. m)
(Neg n) .+. (Neg m) 		   = Neg (Succ n +. m)
(Neg n) .+. (Pos m) 		   = Pos m .+. Neg n

(.-.) :: Int -> Int -> Int
n .-. m = n .+. (intNeg m)

infixl 7 .*.
(.*.) :: Int -> Int -> Int
(Pos Zero) .*. _    = Pos Zero
_ .*. (Pos Zero)    = Pos Zero
(Pos n) .*. (Pos m) = Pos (n *. m)
(Pos n) .*. (Neg m) = intNeg (Pos(n *. (Succ m)))
(Neg n) .*. (Pos m) = intNeg (Pos((Succ n) *. m))
(Neg n) .*. (Neg m) = Pos ((Succ n) *. (Succ m))

-------------------------------------------
-- Рациональные числа

data Rat = Rat Int Nat

ratNeg :: Rat -> Rat
ratNeg (Rat x y) = Rat (intNeg x) y

ratInv :: Rat -> Rat
ratInv (Rat (Pos Zero) _) = error "Division by zero"
ratInv (Rat (Pos n) m) = Rat (Pos m) n 
ratInv (Rat (Neg n) m) = Rat (intNeg (Pos m)) (Succ n)

-- Дальше как обычно
ratCmp :: Rat -> Rat -> Tri
ratCmp (Rat n m) (Rat x y) = intCmp (n .*. (Pos y)) (x .*. (Pos m))

ratEq :: Rat -> Rat -> Bool
ratEq (Rat n m) (Rat x y) = intEq (n .*. (Pos y)) (x .*. (Pos m))

ratLt :: Rat -> Rat -> Bool
ratLt (Rat n m) (Rat x y) = intLt (n .*. (Pos y)) (x .*. (Pos m))

infixl 7 %+, %-
(%+) :: Rat -> Rat -> Rat
(Rat n m) %+ (Rat x y) = Rat ((n .*. (Pos y)) .+. (x .*. (Pos m))) (m *. y)

(%-) :: Rat -> Rat -> Rat
n %- m = n %+ (ratNeg m)

infixl 7 %*, %/
(%*) :: Rat -> Rat -> Rat
(Rat n m) %* (Rat x y) = Rat (n .*. x) (m *. y)

(%/) :: Rat -> Rat -> Rat
n %/ m = n %* (ratInv m)
-------------------------------------------
-- Операции над функциями.
-- Определены здесь, но использовать можно и выше

infixr 9 .
f . g = \ x -> f (g x)

infixr 0 $
f $ x = f x

-- Эквивалентные определения
example3   a b c = gcd a (gcd b c)
example3'  a b c = gcd a $ gcd b c
example3'' a b c = ($) (gcd a) (gcd b c)

-- И ещё эквивалентные определения
example4  a b x = (gcd a (gcd b x))
example4' a b = gcd a . gcd b