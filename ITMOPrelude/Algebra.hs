{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Algebra where

-- ����������� ��� �����,
-- ��� ������ ����� ��
import ITMOPrelude.Primitive
-- ������������ �������� ��� ������� ���� 

-- ���� �� �������, �� ���������� �� � ���
--import ITMOPrelude.List
--import ITMOPrelude.Tree

-- ������
class Monoid a where
    mempty :: a
    mappend :: a -> a -> a

class Monoid a => Group a where
    ginv :: a -> a

-- �������� ������ ����

instance Monoid Nat where
	mempty = Zero
 	mappend = (+.)


instance Monoid Int where
	mempty = intZero
	mappend = (.+.)

newtype IntMul = IntMul Int

instance Monoid IntMul where
    mempty = IntMul intOne
    mappend (IntMul a) (IntMul b) = IntMul (a .*. b)


instance Monoid Rat where
	mempty = Rat intZero natOne
	mappend = (%+)

newtype RatMul = RatMul Rat

instance Monoid RatMul where
    mempty = RatMul (Rat intOne natOne)
    mappend (RatMul a) (RatMul b) = RatMul (a %* b)
                                      
instance Group Int where
	 ginv = intNeg

                                 
instance Group Rat where
	ginv = ratNeg

instance Group RatMul where
    ginv (RatMul a) = RatMul (ratInv a)

