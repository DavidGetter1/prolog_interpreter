module Unifikation where

import Data.List (nub)
import Pretty
import Substitutionen
import Test.QuickCheck
import Type
import Variablen

-- Definieren Sie als erstes eine Funktion die die Unstimmigkeitsmenge zweier Terme berechnet und sie als Paar zurückgibt.
-- Sollte die Unstimmigkeitsmenge leer sein, soll die Funktion stattdessen Nothing zurückgeben. Berücksichtigen Sie dabei auch anonyme Variablen (_) sinnvoll.
ds :: Term -> Term -> Maybe (Term, Term)
ds (Comb name1 termList1) (Comb name2 termList2)
  | (name1 /= name2) || length termList1 /= length termList2 = Just (Comb name1 termList1, Comb name2 termList2)
  | name1 == name2 && length termList1 == length termList2 = case findDisTupel (zip termList1 termList2) of
    Nothing -> Nothing
    Just (t1, t2) -> uncurry ds (t1, t2)
  where
    -- returns the first tupel where ts /= tk (see ds definition in script)
    findDisTupel :: [(Term, Term)] -> Maybe (Term, Term)
    findDisTupel [] = Nothing
    -- findDisTupel ((Comb name tlist) : ts) = ds Comb name tlist
    findDisTupel (t : ts) = if fst t /= snd t then Just t else findDisTupel ts
ds t1 t2 = case (t1, t2) of
  (Var t1, Var t2) -> if t1 == t2 || t2 == VarName "_" || t1 == VarName "_" then Nothing else Just (Var t1, Var t2)
  (Comb _ _, Var t2) -> Just (t1, Var t2)
  (Var t1, Comb _ _) -> Just (Var t1, t2)
  (Comb _ _, Comb _ _) -> Nothing -- already covered

-- unify algorithm
unify :: Term -> Term -> Maybe Subst
unify t1 t2
  | substToList (unifyGen t1 t2 (Subst [])) == [] = Nothing
  | otherwise = Just (unifyGen t1 t2 (Subst []))

--
unifyGen :: Term -> Term -> Subst -> Subst
unifyGen t1 t2 mgu
  | ds (apply mgu t1) (apply mgu t2) == Nothing = mgu
  | ds (apply mgu t1) (apply mgu t2) /= Nothing = case (t1, t2) of
    (Var t1, Var t2) -> if t1 == t2 then Subst [] else Subst [(t1, Var t2)]
    (Comb name1 termList1, Var t2) -> if t2 `elem` (concatMap allVars termList1) then Subst [] else unifyGen t1 (Var t2) newMgu
    (Var t2, Comb name1 termList1) -> if t2 `elem` (concatMap allVars termList1) then Subst [] else unifyGen t1 (Var t2) newMgu
    (Comb _ _, Comb t2 _) -> Subst []
  where
    newMgu = ((dsToSubst (ds (apply mgu t1) (apply mgu t2))) `compose` mgu)

-- converts a disagreement set to a substitution
dsToSubst :: Maybe (Term, Term) -> Subst
dsToSubst Nothing = Subst []
dsToSubst (Just (Var x, Var y)) = Subst [(x, Var y)]
dsToSubst (Just (Var x, Comb name termList)) = Subst [(x, Comb name termList)]
dsToSubst (Just (Comb name termList, Var x)) = Subst [(x, Comb name termList)]
dsToSubst _ = Subst [] -- never reached

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
  | ds t1 t2 == Nothing =
    substToList (unMaybe (unify t1 t2)) == []
      && ( case unify t1 t2 of
             Nothing -> True
             Just su -> domain su == []
         )
  | otherwise = True

--unify(t1,t2) ≠ fail⇒ds(apply(unify(t1,t2),t1),apply(unify(t1,t2),t2)) = {}
prop_lawU4 :: Term -> Term -> Bool
prop_lawU4 t1 t2
  | substToList (unMaybe (unify t1 t2)) /= [] = ds (apply (unMaybe (unify t1 t2)) t1) (apply (unMaybe (unify t1 t2)) t2) == Nothing
  | otherwise = True

-- converts Maybe Subst to Subst
unMaybe :: Maybe Subst -> Subst
unMaybe su = case su of Nothing -> Subst []; Just su -> su