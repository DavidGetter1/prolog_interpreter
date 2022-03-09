module SLD where

import Data.List
import Data.Maybe
import Debug.Trace
import Rename
import Substitutionen
import Type
import Unifikation
import Variablen

-- Data type representation for SLD tree with Goal as nodes and lists of (edge, child)
--             Goal [(outgoing edge with mgu, child)] or Failure
data SLDTree = Node Goal [(Subst, SLDTree)] | Fail
  deriving (Show)

type Strategy = SLDTree -> [Subst]

-- typeclass for returning names of rules and terms
class Name a where
  getName :: a -> String

instance Name Rule where
  getName (Rule term termList) = getName term

instance Name Term where
  getName (Var (VarName name)) = name
  getName (Comb combName _) = combName

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

--trace "hallo" (Node (Goal (t : ts)) (map (sldHelper usedVars (Prog ruleList') (Goal (t : ts))) (filter (\x -> getName t `isInfixOf` getName x && getListLength t == getListLength x) ruleList')))

sld :: Prog -> Goal -> SLDTree
--call sld helper function with empty list of forbidden variablenames
sld = sld' []

sld' :: [VarName] -> Prog -> Goal -> SLDTree
sld' usedVars _ (Goal []) = Node (Goal []) []
sld' usedVars (Prog ruleList) (Goal (t : ts)) = sld'' (allVars (Goal (t : ts))) (Prog ruleList) (Goal (t : ts))
  where
    -- rename all variables of program to avoid name conflicts
    ruleList' = map (rename (usedVars ++ allVars (Goal (t : ts)))) ruleList
    sld'' :: [VarName] -> Prog -> Goal -> SLDTree
    sld'' _ (Prog _) (Goal []) = Node (Goal []) [] -- empty goal -> success!
    sld'' _ (Prog []) goal = Node goal [] -- empty program
    sld'' usedVars (Prog rs) (Goal (t2 : ts2)) = Node (Goal (t2 : ts2)) (filter (notFail . snd) ts2')
      where
        -- modified ts2 aqcuired by looking for a matching rule
        ts2' = map findRuleMatch ruleList'
          where
            -- returns pair of outgoing mgu and children node for first rule matching with term of goal
            -- returns pair of empty and Fail if no rule matches
            findRuleMatch = \(Rule ruleTerm ruleTermList) -> case unify t2 ruleTerm of -- unify checks implicitly whether t2 fits to ruleTerm and returns their Maybe mgu
              Just subst -> (subst, sld'' (usedVars ++ allVars subst ++ allVars (Goal (t2 : ts2))) (Prog ruleList) (Goal (map (apply subst) (ruleTermList ++ ts2)))) -- recursive sld'' call with the new Goal where the usedVars are passed and expanded
              Nothing -> (empty, Fail) -- rule didn't match, unify algo returns fail for this node

-- returns True if passed SLD Node isn't Fail
notFail :: SLDTree -> Bool
notFail Fail = False
notFail _ = True

-- sld (Prog [(Rule (Comb "q" [Comb "b" []]) []), (Rule (Comb "p" [Comb "q" []]) [Comb "q" [Var (VarName "X")]])]) (Goal [Comb "p" [Var(VarName "X")]])

dfs :: Strategy
--dfs (Node (Goal []) _) = [empty]  -- may need
--dfs (Node _ []) = [empty]         -- may need
dfs (Node (Goal ts) (node : nodes)) = map (\x -> restrictTo x (allVars (Goal ts))) (map (\x -> foldr compose empty (((dfs (snd x))) ++ [fst x])) (node : nodes))
dfs Fail = [empty] -- Fail doesn't occur in tree computed by sld
dfs _ = [empty] -- possibly never reached

--type Strategy = SLDTree -> [Subst]

solveWith :: Prog -> Goal -> Strategy -> [Subst]
solveWith prog goal strat = map (\x -> restrictTo x (allVars goal)) (strat (sld prog goal))

bfs :: Strategy
bfs tree = bfs' [(empty, tree)]
  where
    bfs' :: [(Subst, SLDTree)] -> [Subst]
    bfs' [] = []
    bfs' (q : qs)
      | null (getKids (snd q)) = if isSuccess (snd q) then fst q : bfs' qs else bfs' qs
      | otherwise = bfs' (qs ++ map (\(x, tree) -> (x `compose` fst q, tree)) (getKids (snd q)))

isSuccess :: SLDTree -> Bool
isSuccess (Node (Goal []) []) = True
isSuccess _ = False

getKids :: SLDTree -> [(Subst, SLDTree)]
getKids (Node g k) = k
getKids Fail = []

getSubsts :: SLDTree -> [Subst]
getSubsts (Node g k) = map fst k
getSubsts Fail = []

{-
--data SLDTree = Node Goal [(Subst, SLDTree)] | Fail -- = leere liste
bfs :: Strategy
bfs (Node (Goal ts) kids) = bfs' [(empty, kids)] []
  where
    -- iterates through levels of tree while composing adjacent substitutions and returning compositions, if subst leads to "successful goal"
    bfs' :: [(Subst, [(Subst, SLDTree)])] -> [Subst] -> [Subst]
    bfs' [(_, [])] resultList = trace (show resultList ++ "HALLO") resultList
    bfs' list (resultList) = bfs' (delAllSuccesses substCNodes) (getAllSuccesses (resultList) substCNodes)
      where
        --concatMap (\(upS, list) -> map (upS `compose`) (map fst list)) (zip (map (upperSubst `compose`) (map fst nodes)) listOfChildren)
        substCNodes = (zip (concatMap (\(subst, cNodes) -> map (\x -> x `compose` subst) (map fst cNodes)) list) listOfChildren)
        listOfChildren = map (\(Node g cNodes) -> cNodes) (map snd kids)

--remove all entries from list that have a succesful goal inside its pair
delAllSuccesses :: [(Subst, [(Subst, SLDTree)])] -> [(Subst, [(Subst, SLDTree)])]
delAllSuccesses list = filter (\(x, liste) -> not (null liste)) list

--gets all successfull substitutions
getAllSuccesses :: [Subst] -> [(Subst, [(Subst, SLDTree)])] -> [Subst]
getAllSuccesses listToAdd listToTakeFrom = listToAdd ++ map fst (filter (\(x, liste) -> null liste) listToTakeFrom)
-}

--[(Subst, [(Subst, SLDTree)])]

-- idee: gebe kind liste seiner geschwister, wenn kind selbst das letzte geschwister aus der lsite ist, haben wir die ebene fertig bearbeitet
-- bfs node = map (liste von substs)
-- map bfs (liste v susbst)

--Prog [Rule (Comb "auto" [Var (VarName "X"), Var (VarName "X")]) [],(Rule (Comb "p" [Var (VarName "X"), Var (VarName "Z")]) [(Comb "q" [Var (VarName "X"), Var (VarName "Y")]),(Comb "p" [Var (VarName "Y"), Var (VarName "Z")])]),Rule (Comb "p" [Var (VarName "X"), Var (VarName "X")]) [],Rule (Comb "q" [Comb  "a" [], Comb "b" []]) []]
-- Comb "p" [Var (VarName "S"), (Comb "b" [])],

-- sld (Prog [Rule (Comb "auto" [Var (VarName "X"), Var (VarName "X")]) [],(Rule (Comb "p" [Var (VarName "X"), Var (VarName "Z")]) [(Comb "q" [Var (VarName "X"), Var (VarName "Y")]),(Comb "p" [Var (VarName "Y"), Var (VarName "Z")])]),Rule (Comb "p" [Var (VarName "X"), Var (VarName "X")]) [],Rule (Comb "q" [Comb  "a" [], Comb "b" []]) []]) (Goal [Comb "p" [Var (VarName "S"),Comb "b" []]])

-- converts a maybe to its just value or the given default in the first argument if it is nothing
unMaybe2 :: a -> Maybe a -> a
unMaybe2 a may = case may of
  Just x -> x
  Nothing -> a

-- solveWith ((Prog [Rule (Comb "auto" [Var (VarName "X"), Var (VarName "X")]) [],(Rule (Comb "p" [Var (VarName "X"), Var (VarName "Z")]) [(Comb "q" [Var (VarName "X"), Var (VarName "Y")]),(Comb "p" [Var (VarName "Y"), Var (VarName "Z")])]),Rule (Comb "p" [Var (VarName "X"), Var (VarName "X")]) [],Rule (Comb "q" [Comb  "a" [], Comb "b" []]) []])) (Goal [Comb "p" [Var (VarName "S"),Comb "b" []]]) (bfs)