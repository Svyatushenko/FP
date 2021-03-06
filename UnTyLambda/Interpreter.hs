﻿{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- В данном задании требуется реализовать интерпретатор для
-- нетипизированной лямбды
--------------------------------------------------------------------------------

module UnTyLambda.Interpreter where

-- Какие-то импорты. Заметьте, что в этом задании надо
-- использовать обычную Prelude
import Prelude hiding (catch)
import Control.Exception

------------------------------------------------------------
-- Определение дататайпа для нетипизированной лямбды
type Variable = String
data Term = Var Variable | Lam Variable Term | App Term Term deriving (Show,Read)

------------------------------------------------------------
-- Дальше всё на ваше усмотрение

-- Если внутри будете использовать именованное представление, то
-- я тут решил немного вам помочь
-- (иначе говоря, код из этого раздела можно совсем выкинуть,
-- если хочется)

free (Var v) = [ v ]
free (Lam v t) = filter (/= v) . free $ t
free (App t t') = (free t) ++ (free t')

subst :: Term -> Variable -> Term -> Term
subst t@(Var v) var what = if v == var then what else t
subst t@(Lam v b) var what = if v == var then t else Lam v (subst b var what)
subst (App t t') var what = App (subst t var what) (subst t' var what)

newname fv = head . filter (not . flip elem fv) . iterate ('_':)


betaRed :: Term -> Variable -> Term -> Term
betaRed t@(Var v) var what 	= if v == var then what else t
betaRed (App t t') var what 	= App (betaRed t var what) (betaRed t' var what)
betaRed (Lam v t) var what 	| var == v 			= Lam v t
				| not (elem v (free what)) 	= Lam v (betaRed t var what)
				| otherwise 			= Lam v' (betaRed (subst t v (Var v')) var what)
					where v' = newname ((free what) ++ (free t)) v


------------------------------------------------------------
-- За исключением того, что требуется реализовать следующие
-- стратегии нормализации (они все принимают максимальное
-- число шагов интерпретатора в качестве первого
-- параметра (n); если за n шагов нормализовать не удаётся,
-- то следует бросать error, тестер его поймает):

wh, no, wa, sa :: Integer -> Term -> Term

-- Редукция аппликативным порядком
sa 0 t = error $ "Too long sequence at [" ++ show t ++ "]"
sa n t = 	if fst tmp
		then sa (n - 1) (snd tmp)
		else snd tmp
			where tmp = sa' t
				where sa' (Lam var term) 		= (fst tmp, Lam var (snd tmp))
										where 	tmp = sa' term
				      sa' (App (Lam var term) t) 	= if fst tmp
									  then (True, App (Lam var (snd tmp)) t)
									  else (True, betaRed term var t)
										where 	tmp = sa' term
				      sa' (App t1 t2) 			= if fst tmp1
									  then (True, App (snd tmp1) t2)
									  else (fst tmp2, App t1 (snd tmp2))
										where 	tmp1 = sa' t1
											tmp2 = sa' t2
				      sa' term 				= (False, term)


-- Нормализация нормальным порядком
no 0 t = error $ "Too long sequence at [" ++ show t ++ "]"
no n t = 	if fst tmp
	 	then no (n - 1) (snd tmp)
		else snd tmp
			where tmp = no' t
				where 	no' (Lam var term) 		= (fst tmp, Lam var (snd tmp))
										where 	tmp = no' term
					no' (App (Lam var term) t) 	= (True, betaRed term var t)
					no' (App t1 t2) 		= if fst tmp1
									  then (True, App (snd tmp1) t2)
									  else (fst tmp2, App t1 (snd tmp2))
										where 	tmp1 = no' t1
											tmp2 = no' t2
					no' term 			= (False, term)

-- Редукция в слабую головную нормальную форму
wh 0 t = error $ "Too long sequence at [" ++ show t ++ "]"
wh n t = 	if fst tmp
		then wh (n - 1) (snd tmp)
		else snd tmp
			where tmp = wh' t
				where 	wh' (App (Lam var term) t) 	= (True, betaRed term var t)
					wh' (App t1 t2) 		= if fst tmp1
									  then (True, App (snd tmp1) t2)
									  else (fst tmp2, App t1 (snd tmp2))
										where 	tmp1 = wh' t1
											tmp2 = wh' t2
					wh' term 			= (False, term)

-- (*) (не обязательно) Редукция "слабым" аппликативным порядком.
-- Отличается от обычного аппликативного тем, что не лезет внутрь
-- лямбд и правые части аппликаций, когда это возможно.
wa = undefined

-- Замечание: cкорость работы вашего интерпретатора специально не оценивается,
-- потому можно использовать свой изоморфный (с точностью до альфа-конверсии)
-- тип для представления термов и преобразовывать Term в него и обратно.

-- Перечисление всех этих порядков (в порядке отличном от
-- определения, да)
orders =
    [ ("wh", wh)
    , ("no", no)
-- , ("wa", wa) -- Можно раскоментировать, да
    , ("sa", sa) ]

------------------------------------------------------------
-- Игнорируйте это, если выглядит непонятно
pall term = mapM_ $ \(d, x) -> putStr (d ++ ": ") >> catch (let t = x 1000 term in seq t (print t)) (\(e :: SomeException) -> print e)
testfuncs funcs = mapM_ $ \t -> putStr "===== " >> print t >> pall t funcs

------------------------------------------------------------
-- Сюда можно добавлять тесты
lamxx = Lam "x" $ App (Var "x") (Var "x")
omega = App lamxx lamxx

test = testfuncs orders
    [ Var "a"
    , Lam "x" $ (Lam "y" $ Var "y") `App` (Var "x")
    , (Lam "x" $ Lam "y" $ Var "x") `App` (Var "y")
    , omega
    ]

------------------------------------------------------------
-- Немного теоретических замечаний, если они вас волнуют
--
-- Следует специально отметить, что поскольку в конце теста
-- результат вычисления печатают, то ленивость Haskell не
-- влияет на семантику интерпретируемого исчисления.
--
-- Чтобы это особенно подчеркнуть в тестере выше я написал
-- seq в интересном месте (хотя конкретно это там ничего не
-- гарантирует, на самом-то деле).