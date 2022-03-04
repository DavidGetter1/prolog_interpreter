module Unifikation where

import Data.List
import Data.Maybe
import Substitutionen
import Type
import Variablen

-- (zipWith ds termList1 termList2) !! (findindex Just _)

-- Definieren Sie als erstes eine Funktion die die Unstimmigkeitsmenge zweier Terme berechnet und sie als Paar zurückgibt.
-- Sollte die Unstimmigkeitsmenge leer sein, soll die Funktion stattdessen Nothing zurückgeben. Berücksichtigen Sie dabei auch anonyme Variablen (_) sinnvoll.
ds :: Term -> Term -> Maybe (Term, Term)
ds (Comb name1 termList1) (Comb name2 termList2)
  | (name1 /= name2) || length termList1 /= length termList2 = Just (Comb name1 termList1, Comb name2 termList2)
  | otherwise = fromMaybe Nothing (find isJust (zipWith ds termList1 termList2))
--where
--   Nothing -> Nothing
--   Just (t1, t2) -> uncurry ds (t1, t2)

-- returns the first tupel where ts /= tk (see ds definition in script)
--  findDisTupel :: [(Term, Term)] -> Maybe (Term, Term)
--  findDisTupel [] = Nothing
--  -- findDisTupel ((Comb name tlist) : ts) = ds Comb name tlist
--  findDisTupel (t : ts) = if fst t /= snd t then Just t else findDisTupel ts
ds t1 t2 = case (t1, t2) of
  (Var t6, Var t5) -> if t6 == t5 || t5 == VarName "_" || t6 == VarName "_" then Nothing else Just (Var t6, Var t5)
  (Comb _ _, Var t4) -> Just (t1, Var t4)
  (Var t3, Comb _ _) -> Just (Var t3, t2)
  (Comb _ _, Comb _ _) -> Nothing -- already covered

-- unify algorithm
unify :: Term -> Term -> Maybe Subst
unify t1 t2
  | null (substToList (unifyGen t1 t2 empty)) = Nothing
  | otherwise = Just (unifyGen t1 t2 empty)

--
unifyGen :: Term -> Term -> Subst -> Subst
unifyGen t1 t2 mgu
  | isNothing (ds (apply mgu t1) (apply mgu t2)) = mgu
  | isJust (ds (apply mgu t1) (apply mgu t2)) = case (t1, t2) of
    (Var t6, Var t5) -> if t6 == t5 then empty else single t6 (Var t5)
    (Comb _ termList1, Var t4) -> if t4 `elem` concatMap allVars termList1 then empty else unifyGen t1 (Var t4) newMgu
    (Var t3, Comb _ termList1) -> if t3 `elem` concatMap allVars termList1 then empty else unifyGen t1 (Var t3) newMgu
    (Comb _ _, Comb _ _) -> empty
  where
    newMgu = dsToSubst (ds (apply mgu t1) (apply mgu t2)) `compose` mgu
unifyGen _ _ _ = empty -- already covered above

-- converts a disagreement set to a substitution
dsToSubst :: Maybe (Term, Term) -> Subst
dsToSubst Nothing = empty
dsToSubst (Just (Var x, Var y)) = single x (Var y)
dsToSubst (Just (Var x, Comb name termList)) = single x (Comb name termList)
dsToSubst (Just (Comb name termList, Var x)) = single x (Comb name termList)
dsToSubst _ = empty -- never reached

--ds(t,t) = {}
prop_lawU1 :: Term -> Bool
prop_lawU1 t = ds t t == Nothing

--ds(t1,t2) ≠ {}⇒t1 ≠ t2
prop_lawU2 :: Term -> Term -> Bool
prop_lawU2 t1 t2
  | ds t1 t2 /= Nothing = t1 /= t2
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
