{-# LANGUAGE TemplateHaskell #-}

module Rename where

import Substitutionen
import Test.QuickCheck
import Type
import Variablen

--renames all variables in the passed rule, that are elements of the passed list
--first renames all anonymous variables individually (using renameAnon),
--then renames all of the other variables (using renameHelper)
rename :: [VarName] -> Rule -> Rule
rename varList (Rule term termList) = renameHelper varList (renameAnon (varList ++ allVars (Rule term termList)) (Rule term termList))

--renames all the variables of a rule that has no anonymous variables inside its terms
renameHelper :: [VarName] -> Rule -> Rule
renameHelper varList (Rule term termList) = Rule (apply renameSubst term) (map (apply renameSubst) termList)
  where
    -- renameSubst = Subst (zip (allVars (Rule term termList)) (freshDiffVar varList (allVars (Rule term termList)) []))
    renameSubst = foldr compose empty (zipWith single (allVars (Rule term termList)) (map Var (take (length (allVars (Rule term termList))) (freshDiffVar (varList ++ allVars (Rule term termList))))))

-- returns unused vars
freshDiffVar :: [VarName] -> [VarName]
freshDiffVar used = [x | x <- freshVars, x `notElem` used]

-- takes a list of forbidden names and a rule, then replaces all "_" with different VarNames /= forbidden name
-- each _ gets mapped to a different VarName
renameAnon :: [VarName] -> Rule -> Rule
renameAnon varList (Rule term (t : ts)) --  vvv only apply to fst occ!!!!
  | VarName "_" `elem` allVars term = renameAnon (fVar varList : varList) (Rule (applyToFstAnon (single (VarName "_") (Var (fVar varList))) term) (t : ts))
  | VarName "_" `elem` allVars t = renameAnon (fVar varList : varList) (Rule term (applyToFstAnon (single (VarName "_") (Var (fVar varList))) t : ts))
  | VarName "_" `elem` concatMap allVars ts = Rule term (t : returnTermList (renameAnon varList (Rule term ts)))
  | otherwise = Rule term (t : ts)
renameAnon varList (Rule term []) = if VarName "_" `elem` allVars term then renameAnon (fVar varList : varList) (Rule (apply (single (VarName "_") (Var (fVar varList))) term) []) else Rule term []

-- returns the term list of a rule
returnTermList :: Rule -> [Term]
returnTermList (Rule _ termList) = termList

-- returns one fresh Variable that is not element of the passed list
fVar :: [VarName] -> VarName
fVar varList = head (take 1 (freshDiffVar varList))

-- applies substitution to first occurence of anonymous variable
applyToFstAnon :: Subst -> Term -> Term
applyToFstAnon subst (Var v) = apply subst (Var v)
applyToFstAnon subst (Comb name (t1 : t2 : ts)) = if VarName "_" `elem` allVars t1 then Comb name ((applyToFstAnon subst t1) : t2 : ts) else (Comb name (t1 : (applyToFstAnon subst t2) : ts))
applyToFstAnon _ t = t --never reached when called by renameAnon

-- returns the intersect of two lists
intersection :: Eq a => [a] -> [a] -> [a]
intersection [] _ = []
intersection _ [] = []
intersection xs ys = [x | x <- xs, x `elem` ys]

--allVars(rename(xs,r)) ∩ allVars(r) = {}
prop_lawR1 :: [VarName] -> Rule -> Bool
prop_lawR1 xs r = null (allVars (rename xs r) `intersection` allVars r)

-- allVars(rename(xs,r)) ∩ xs = {}
prop_lawR2 :: [VarName] -> Rule -> Bool
prop_lawR2 xs r = null (allVars (rename xs r) `intersection` xs)

--"_" ∉ allVars(rename(xs,r))
prop_lawR3 :: [VarName] -> Rule -> Bool
prop_lawR3 xs r = VarName "_" `notElem` allVars (rename xs r)

--"_" ∉ allVars(r)⇒|allVars(rename(xs,r))| = |allVars(r)|
prop_lawR4 :: [VarName] -> Rule -> Bool
prop_lawR4 xs r
  | VarName "_" `notElem` allVars r = length (allVars (rename xs r)) == length (allVars r)
  | otherwise = True

-- | allVars(rename(xs,r))| ≥ |allVars(r)|
prop_lawR5 :: [VarName] -> Rule -> Bool
prop_lawR5 xs r = length (allVars (rename xs r)) >= length (allVars r)

return []

runTestsRename :: IO Bool
runTestsRename = $quickCheckAll