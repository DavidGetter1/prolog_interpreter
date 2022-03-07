module Unifikation where

import Data.List
import Data.Maybe
import Substitutionen
import Test.QuickCheck
import Type
import Variablen

-- (zipWith ds termList1 termList2) !! (findindex Just _)

-- Definieren Sie als erstes eine Funktion die die Unstimmigkeitsmenge zweier Terme berechnet und sie als Paar zurückgibt.
-- Sollte die Unstimmigkeitsmenge leer sein, soll die Funktion stattdessen Nothing zurückgeben. Berücksichtigen Sie dabei auch anonyme Variablen (_) sinnvoll.
ds :: Term -> Term -> Maybe (Term, Term)
ds (Comb name1 termList1) (Comb name2 termList2)
  | (name1 /= name2) || length termList1 /= length termList2 = Just (Comb name1 termList1, Comb name2 termList2)
  | otherwise = unMaybe Nothing (find isJust (zipWith ds termList1 termList2))
ds t1 t2 = case (t1, t2) of
  (Var t6, Var t5) -> if t6 == t5 || t5 == VarName "_" || t6 == VarName "_" then Nothing else Just (Var t6, Var t5)
  (Comb _ _, Var t4) -> Just (t2, t1)
  (Var t3, Comb _ _) -> Just (t1, t2)
  (Comb _ _, Comb _ _) -> Nothing -- already covered

-- converts a maybe to its just value or the given default in the first argument if it is nothing
unMaybe :: a -> Maybe a -> a
unMaybe a may = case may of
  Just x -> x
  Nothing -> a

-- unify algorithm
unify :: Term -> Term -> Maybe Subst
unify t1 t2
  | null (substToList (unifyGen t1 t2 empty)) = Nothing
  | otherwise = Just (unifyGen t1 t2 empty)

-- helps with recursion of unify algorithm
unifyGen :: Term -> Term -> Subst -> Subst
unifyGen t1 t2 mgu = case ds (apply mgu t1) (apply mgu t2) of -- hier werden ganze terme verglichen, nicht die elemente des ds!!!
  Just (Var t6, Var t5) -> if t6 == t5 then empty else unifyGen t1 t2 (single t6 (Var t5) `compose` mgu)
  Just (Var t3, Comb cName termList1) -> if t3 `elem` concatMap allVars termList1 then empty else unifyGen t1 t2 (single t3 (Comb cName termList1) `compose` mgu)
  Just (Comb _ _, _) -> empty
  Nothing -> mgu

--ds(t,t) = {}
prop_lawU1 :: Term -> Bool
prop_lawU1 t = isNothing (ds t t)

--ds(t1,t2) ≠ {}⇒t1 ≠ t2
prop_lawU2 :: Term -> Term -> Bool
prop_lawU2 t1 t2
  | isJust (ds t1 t2) = t1 /= t2
  | otherwise = True

--ds(t1,t2) = {}⇒unify(t1,t2) ≠ fail ∧ domain(unify(t1,t2)) = {}
prop_lawU3 :: Term -> Term -> Bool
prop_lawU3 t1 t2
  | isNothing (ds t1 t2) =
    null (substToList (fromMaybe empty (unify t1 t2)))
      && ( case unify t1 t2 of
             Nothing -> True
             Just su -> null (domain su)
         )
  | otherwise = True

--unify(t1,t2) ≠ fail⇒ds(apply(unify(t1,t2),t1),apply(unify(t1,t2),t2)) = {}
prop_lawU4 :: Term -> Term -> Bool
prop_lawU4 t1 t2
  | substToList (fromMaybe empty (unify t1 t2)) /= [] = isNothing (ds (apply (fromMaybe empty (unify t1 t2)) t1) (apply (fromMaybe empty (unify t1 t2)) t2))
  | otherwise = True
