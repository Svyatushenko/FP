module ITMOPrelude.Primitive where

import Prelude (Show,Read,error)

undefined = undefined
-------------------------------------------
-- ����������� ����

-- ��� � ������������ ���������
data Unit = Unit deriving (Show,Read)

-- ����, ������������
data Pair a b = Pair { fst :: a, snd :: b } deriving (Show,Read)

-- �������, ��������������
data Either a b = Left a | Right b deriving (Show,Read)

-- ������ ������� ������, ��������� Either Unit a
data Maybe a = Nothing | Just a deriving (Show,Read)

-- ������ ������� ������, ��������� Either Unit Unit
data Bool = False | True deriving (Show,Read)

-- ������� ��������, ��� ���������� if � ���� Bool ������������ ������,
-- ���� case ������ ��������.

-- �� ��� ����� ����������� ���� if
if' True a b = a
if' False a b = b

-- ����������. ������������� ���, ������������ ��������� ���������
data Tri = LT | EQ | GT deriving (Show,Read)

-------------------------------------------
-- ������ ��������

-- ���������� "��"
not :: Bool -> Bool
not True = False
not False = True

infixr 3 &&
-- ���������� "�"
(&&) :: Bool -> Bool -> Bool
True  && x = x
False && _ = False

infixr 2 ||
-- ���������� "���"
(||) :: Bool -> Bool -> Bool
True  || _ = True
False || x = x

-------------------------------------------
-- ����������� �����

data Nat = Zero | Succ Nat deriving (Show,Read)

natZero = Zero     -- 0
natOne = Succ Zero -- 1

-- ���������� ��� ����������� �����
natCmp :: Nat -> Nat -> Tri
natCmp n m = if' (natEq n m) (EQ) (if' (natLt n m) (LT) (GT))

-- n ��������� � m 
natEq :: Nat -> Nat -> Bool
natEq Zero     Zero     = True
natEq Zero     (Succ _) = False
natEq (Succ _) Zero     = False
natEq (Succ n) (Succ m) = natEq n m

-- n ������ m
natLt :: Nat -> Nat -> Bool
natLt Zero     Zero     = False
natLt Zero     (Succ m) = True
natLt (Succ n) Zero     = False
natLt (Succ n) (Succ m) = natLt n m

-- n ������ m
natGt :: Nat -> Nat -> Bool
natGt Zero     Zero     = False
natGt Zero     (Succ m) = False
natGt (Succ n) Zero     = True
natGt (Succ n) (Succ m) = natGt n m

infixl 6 +.
-- �������� ��� ����������� �����
(+.) :: Nat -> Nat -> Nat
Zero     +. m = m
(Succ n) +. m = Succ (n +. m)

infixl 6 -.
-- ��������� ��� ����������� �����
(-.) :: Nat -> Nat -> Nat
Zero	 -. _ = Zero
(Succ n) -. Zero = (Succ n)
(Succ n) -. (Succ m) = n -. m

infixl 7 *.
-- ��������� ��� ����������� �����
(*.) :: Nat -> Nat -> Nat
Zero     *. m = Zero
(Succ n) *. m = m +. (n *. m)

-- ����� � ������� �� ������� n �� m
natDivMod :: Nat -> Nat -> Pair Nat Nat
natDivMod n Zero = error "Division by zero"
natDivMod n m = if' (natLt n m) (Pair Zero n) (Pair (Succ $ natDiv (n -. m) m) (natMod (n -. m) m))

natDiv n = fst . natDivMod n -- �����
natMod n = snd . natDivMod n -- �������

-- ����� GCD ���������� ������� (������ �������� 2 (���������������� �����) + 1 (���) �������)
gcd :: Nat -> Nat -> Nat
gcd n Zero = n
gcd n m    = gcd m (natMod n m)

-------------------------------------------
-- ����� �����

-- ���������, ����� ������������� ������� ����� ���� ������������
data Int = Pos Nat | Neg Nat deriving (Show,Read)

intZero   = Pos Zero   -- 0
intOne    = Pos (Succ Zero)     -- 1
intNegOne = Neg Zero -- -1

-- n -> - n
intNeg :: Int -> Int
intNeg (Pos (Succ n)) = Neg n
intNeg (Neg n)        = Pos (Succ n)
intNeg (Pos Zero)     = Pos Zero


-- ������ ����� ��� ��� �����������
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
-- � ���� ��� ������������ �������� ���� �� ��� �����
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
-- ������������ �����

data Rat = Rat Int Nat

ratNeg :: Rat -> Rat
ratNeg (Rat x y) = Rat (intNeg x) y

ratInv :: Rat -> Rat
ratInv (Rat (Pos Zero) _) = error "Division by zero"
ratInv (Rat (Pos n) m) = Rat (Pos m) n 
ratInv (Rat (Neg n) m) = Rat (intNeg (Pos m)) (Succ n)

-- ������ ��� ������
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
-- �������� ��� ���������.
-- ���������� �����, �� ������������ ����� � ����

infixr 9 .
f . g = \ x -> f (g x)

infixr 0 $
f $ x = f x

-- ������������� �����������
example3   a b c = gcd a (gcd b c)
example3'  a b c = gcd a $ gcd b c
example3'' a b c = ($) (gcd a) (gcd b c)

-- � ��� ������������� �����������
example4  a b x = (gcd a (gcd b x))
example4' a b = gcd a . gcd b