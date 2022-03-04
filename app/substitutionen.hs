module Substitutionen where

import Data.List
import Pretty
import Test.QuickCheck
import Type
import Variablen

-- data type for rerpesenting a substituion
data Subst = Subst [(VarName, Term)]
  deriving (Show)

instance Pretty Subst where
  pretty (Subst s1) = reverse (tail (tail (reverse ("{" ++ concatMap (\x -> pretty (fst x) ++ " -> " ++ pretty (snd x) ++ ", ") s1)))) ++ "}"

instance Vars Subst where
  allVars (Subst s1) = nub (concatMap (\(x, y) -> x : allVars y) s1)

instance Arbitrary Subst where
  arbitrary = do
    name <- arbitrary
    term <- arbitrary `suchThat` (\x -> x /= Var name)
    return (Subst [(name, term)])

-- apply(empty,t) = t
prop_law1 :: Term -> Bool
prop_law1 t = apply empty t == t

-- apply(single(x,t),Var x) = t
prop_law2 :: VarName -> Term -> Bool
prop_law2 x t = apply (single x t) (Var x) == t

--apply(compose(s1,s2),t) = apply(s1,apply(s2,t))
prop_law3 :: Subst -> Subst -> Term -> Bool
prop_law3 s1 s2 t = apply (compose s1 s2) t == apply s1 (apply s2 t)

prop_law4 :: VarName -> Bool
prop_law4 x = domain empty == []

--domain(single(x,Var x)) = {}
prop_law5 :: VarName -> Bool
prop_law5 x = domain (single x (Var x)) == []

-- t ≠ Var x ⇒ domain(single(x,t)) = {x}
prop_law6 :: VarName -> Term -> Bool
prop_law6 x term
  | Var x /= term = domain (single x term) == [x]
  | otherwise = True

--domain(compose(s1,s2)) ⊆ domain(s1) ∪ domain(s2)
prop_law7 :: Subst -> Subst -> Bool
prop_law7 s1 s2 = filter (`notElem` (domain s1 ++ domain s2)) (domain (compose s1 s2)) == []

--x1 ≠ x2⇒domain(compose(single(x2,Var x1),single(x1,Var x2))) = {x2}
prop_law8 :: VarName -> VarName -> Bool
prop_law8 x1 x2
  | x1 /= x2 = domain (compose (single x2 (Var x1)) (single x1 (Var x2))) == [x2]
  | otherwise = True

--allVars(empty) = {}
prop_law9 :: Bool
prop_law9 = allVars empty == []

--allVars(single(x,Var x)) = {}
prop_law10 :: VarName -> Bool
prop_law10 x = allVars (single x (Var x)) == []

-- t ≠ Var x ⇒ allVars(single(x,t)) = allVars(t) ∪ {x}
prop_law11 :: VarName -> Term -> Bool
prop_law11 x t
  | t /= Var x = allVars (single x t) == nub (x : allVars t)
  | otherwise = True

--allVars(compose(s1,s2)) ⊆ allVars(s1) ∪ allVars(s2)
prop_law12 :: Subst -> Subst -> Bool
prop_law12 s1 s2 = filter (`notElem` (allVars s1 ++ allVars s2)) (allVars (compose s1 s2)) == []

--x1 ≠ x2⇒allVars(compose(single(x2,Var x1),single(x1,Var x2))) = {x1,x2}
prop_law13 :: VarName -> VarName -> Bool
prop_law13 x1 x2
  | x1 /= x2 = allVars (compose (single x2 (Var x1)) (single x1 (Var x2))) == [x2, x1]
  | otherwise = True

-- domain(s) ⊆ allVars(s)
prop_law14 :: Subst -> Bool
prop_law14 s1 = filter (`notElem` (allVars s1)) (domain s1) == []

--domain(restrictTo(empty,xs)) = {}
prop_law15 :: [VarName] -> Bool
prop_law15 xs = substToList (restrictTo empty xs) == []

-- domain(restrictTo(s,xs)) ⊆ xs
prop_law16 :: [VarName] -> Subst -> Bool
prop_law16 xs s = filter (`notElem` xs) (domain (restrictTo s xs)) == []

-- returns domain of substitution
domain :: Subst -> [VarName]
domain (Subst subst) = map fst subst

empty :: Subst
empty = Subst []

-- creates substitution that maps a single variable to a term
single :: VarName -> Term -> Subst
single varName term
  | Var varName == term = Subst []
  | otherwise = Subst [(varName, term)]

-- applies a substituiton to a term
apply :: Subst -> Term -> Term
apply (Subst []) term = term
apply (Subst (x : xs)) (Var var) = if fst x == var then snd x else apply (Subst xs) (Var var)
apply (Subst (x : xs)) (Comb name termList) = Comb name (map (apply (Subst (x : xs))) termList)

-- helper function for unpack a Subst
substToList :: Subst -> [(VarName, Term)]
substToList (Subst s1) = s1

-- replacing all terms of first substitution with the second subtition applied to them
-- and returning result discarding all pairs (X,X) (maps to itself)
rule1 :: Subst -> Subst -> Subst
rule1 (Subst s1) (Subst s2) = Subst (filter (\(a, b) -> case b of Var b -> a /= b; Comb n b -> True) (zip (map fst s1) (map (apply (Subst s2)) (map snd s1))))

-- returns second substition discarding all pairs (y1,s1) where
-- yi is elem of fst's of first substitution
rule2 :: Subst -> Subst -> Subst
rule2 (Subst s1) (Subst s2) = Subst (filter (\y -> fst y `notElem` xis) s2)
  where
    xis = map fst s1

-- combine rule1 and rule2
compose :: Subst -> Subst -> Subst
compose (Subst s2) (Subst s1) = Subst (substToList (rule1 (Subst s1) (Subst s2)) ++ substToList (rule2 (Subst s1) (Subst s2)))

-- restrict domain of substitution to given VarName list
restrictTo :: Subst -> [VarName] -> Subst
restrictTo (Subst s1) varList = Subst (filter (\x -> fst x `elem` varList) s1)