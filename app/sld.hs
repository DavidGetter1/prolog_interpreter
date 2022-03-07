module SLD where

import Data.List
import Rename
import Substitutionen
import Type
import Unifikation

-- Data type representation for SLD tree with Goal as nodes and lists of (edge, child)
--             Goal [(outgoing edge with mgu, child)] or Failure
data SLDTree = Node Goal [(Maybe Subst, SLDTree)] | Fail
  deriving (Show)

-- typeclass for returning names of rules and terms
class Name a where
  getName :: a -> String

instance Name Rule where
  getName (Rule term termList) = getName term

instance Name Term where
  getName (Var (VarName name)) = name
  getName (Comb combName _) = combName

-- find (\x -> getName t `isPrefixOf` getName x) (r:rs)

class List a where
  getListLength :: a -> Int

instance List Rule where
  getListLength (Rule term termList) = getListLength term

instance List Term where
  getListLength (Var (VarName _)) = 0
  getListLength (Comb _ termList) = length termList

-- constructs the SLD tree for the passed program
-- sld :: Prog -> Goal -> SLDTree
-- sld (Prog (r : rs)) (Goal []) = Node (Goal []) []
-- sld (Prog (r : rs)) (Goal (t : ts)) = case find (\x -> getName t `isInfixOf` getName x && getListLength t == getListLength x) (r : rs) of
--   Nothing -> Fail
--   Just (Rule ruleTerm ruleTermList) -> case unMaybe empty (unify renamedRuleTerm t) of {empty -> Fail; Subst s -> (Node (Goal t) [Just Subst s, (Goal (concatMap (apply (Subst s)) renamedRuleTermList))])}
--     where
--       (Rule renamedRuleTerm renamedRuleTermList) = rename [] (Rule ruleTerm ruleTermList)

-- Just (Rule ruleTerm ruleTermList) -> (case apply ms of {empty -> Fail; s -> s}) t ++ renamedRuleTermList
--   where ms = (case (unify renamedRuleTerm t) of {Just x -> x; Nothing -> empty})

sld :: Prog -> Goal -> SLDTree
sld (Prog ruleList) (Goal (t : ts)) = Node (Goal (t : ts)) (map (sldHelper (Prog ruleList) (Goal (t : ts))) (filter (\x -> getName t `isInfixOf` getName x && getListLength t == getListLength x) ruleList))

sldHelper :: Prog -> Goal -> Rule -> (Maybe Subst, SLDTree)
sldHelper p (Goal (t : ts)) (Rule ruleTerm ruleTermList) =
  case unMaybe2 empty (unify renamedRuleTerm t) of
    empty -> (Just empty, Fail)
    Subst s -> (Just (Subst s), (sld p (Goal ((map (apply (Subst s)) renamedRuleTermList) ++ ts))))
  where
    (Rule renamedRuleTerm renamedRuleTermList) = rename [] (Rule ruleTerm ruleTermList)

-- rename find (\x -> getName t `isInfixOf` getName x && getListLength t == getListLength x)

-- ?- nth(X,[1,2,1],1), nth(Y,[3,4,5],6).

--Prog [Rule (Comb "auto" [Var (VarName "X"), Var (VarName "X")]) [],(Rule (Comb "p" [Var (VarName "X"), Var (VarName "Z")]) [(Comb "q" [Var (VarName "X"), Var (VarName "Y")]),(Comb "p" [Var (VarName "Y"), Var (VarName "Z")])]),Rule (Comb "p" [Var (VarName "X"), Var (VarName "X")]) [],Rule (Comb "q" [Comb  "a" [], Comb "b" []]) []]
-- Comb "p" [Var (VarName "S"), (Comb "b" [])],

-- sld (Prog [Rule (Comb "auto" [Var (VarName "X"), Var (VarName "X")]) [],(Rule (Comb "p" [Var (VarName "X"), Var (VarName "Z")]) [(Comb "q" [Var (VarName "X"), Var (VarName "Y")]),(Comb "p" [Var (VarName "Y"), Var (VarName "Z")])]),Rule (Comb "p" [Var (VarName "X"), Var (VarName "X")]) [],Rule (Comb "q" [Comb  "a" [], Comb "b" []]) []]) (Goal [Comb "p" [Var (VarName "S"),Comb "b" []]])

-- converts a maybe to its just value or the given default in the first argument if it is nothing
unMaybe2 :: a -> Maybe a -> a
unMaybe2 a may = case may of
  Just x -> x
  Nothing -> a