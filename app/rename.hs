module Rename where

import Data.List
import Data.Maybe
import Pretty
import Substitutionen
import Test.QuickCheck
import Type
import Unifikation
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
    renameSubst = Subst (zip (allVars (Rule term termList)) (map Var (take (length (allVars (Rule term termList))) (freshDiffVar (varList ++ allVars (Rule term termList))))))

-- converts a maybe to its just value or the given default in the first argument if it is nothing
unMaybe1 :: a -> Maybe a -> a
unMaybe1 a ma = case ma of
  Just x -> x
  Nothing -> a

-- returns unused vars
freshDiffVar :: [VarName] -> [VarName]
freshDiffVar used = [x | x <- freshVars, x `notElem` used]

-- takes a list of forbidden names and a rule, then replaces all "_" with different VarNames /= forbidden name
-- each _ gets mapped to a different VarName
renameAnon :: [VarName] -> Rule -> Rule
renameAnon varList (Rule term (t : ts))
  | VarName "_" `elem` allVars term = renameAnon (fVar varList : varList) (Rule (apply (single (VarName "_") (Var (fVar varList))) term) (t : ts))
  | VarName "_" `elem` allVars t = renameAnon (fVar varList : varList) (Rule term (apply (single (VarName "_") (Var (fVar varList))) t : ts))
  | VarName "_" `elem` concatMap allVars ts = Rule term (t : returnTermList (renameAnon varList (Rule term ts)))
  | otherwise = Rule term (t : ts)
renameAnon varList (Rule term []) = if VarName "_" `elem` allVars term then renameAnon (fVar varList : varList) (Rule (apply (single (VarName "_") (Var (fVar varList))) term) []) else Rule term []

-- returns the term list of a rule
returnTermList :: Rule -> [Term]
returnTermList (Rule term termList) = termList

-- returns one fresh Variable that is not element of the passed list
fVar :: [VarName] -> VarName
fVar varList = head (take 1 (freshDiffVar varList))

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