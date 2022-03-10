module SLD where

import Data.Maybe
import Rename
import Substitutionen
import Type
import Unifikation
import Variablen

-- Data type representation for SLD tree with Goal as nodes and lists of (edge, child)
--             Goal [(outgoing edge with mgu, child)] or Failure
data SLDTree = Node Goal [(Subst, SLDTree)]
  deriving (Show)

type Strategy = SLDTree -> [Subst]

-- typeclass for returning names of rules and terms
class Name a where
  getName :: a -> String

instance Name Rule where
  getName (Rule term _) = getName term

instance Name Term where
  getName (Var (VarName name)) = name
  getName (Comb combName _) = combName

class List a where
  getListLength :: a -> Int

instance List Rule where
  getListLength (Rule term _) = getListLength term

instance List Term where
  getListLength (Var (VarName _)) = 0
  getListLength (Comb _ termList) = length termList

-- starts dfs' with empty father substitution of root for convenience
-- depth first search
dfs :: Strategy
dfs tree = dfs' empty tree
  where
    -- takes a substitution from father node and composes it with first children node
    -- recursively repeating for its children node until leaf is reached, then for remaining children
    dfs' :: Subst -> SLDTree -> [Subst]
    dfs' upperS (Node (Goal []) _) = [upperS] --success node
    dfs' _ (Node _ []) = [] --fail node
    dfs' upperS (Node _ nodes) = concatMap (\x -> dfs' ((fst x) `compose` upperS) (snd x)) nodes

--type Strategy = SLDTree -> [Subst]

-- returns solutions of sld tree with given strategy
solveWith :: Prog -> Goal -> Strategy -> [Subst]
solveWith prog goal strat = map (\x -> restrictTo x (allVars goal)) (strat (sld prog goal))

-- working with a queue, which gets expanded with the kids of the current node that we are working on
-- breadth first search
bfs :: Strategy
bfs sldTree = bfs' [(empty, sldTree)]
  where
    -- takes a substitution from father node and composes it with children nodes level by level
    -- when current node is success, add substitution to result list
    bfs' :: [(Subst, SLDTree)] -> [Subst]
    bfs' [] = []
    bfs' (q : qs)
      -- checking whether node is leaf, adding subst to result if leaf is success node
      | null (getKids (snd q)) = if isSuccess (snd q) then fst q : bfs' qs else bfs' qs
      -- adds children of current node to the queue and recursively works on that expanded queue
      -- (important: kids get attached to the end of the queue)
      | otherwise = bfs' (qs ++ map (\(x, tree) -> (x `compose` fst q, tree)) (getKids (snd q)))

-- returns if node is success node
isSuccess :: SLDTree -> Bool
isSuccess (Node (Goal []) []) = True
isSuccess _ = False

-- returns all children nodes of an node
getKids :: SLDTree -> [(Subst, SLDTree)]
getKids (Node _ k) = k

--sldtree generator for given Program and Goal
sld :: Prog -> Goal -> SLDTree
sld (Prog rules) g = sld3' (allVars g) (Prog rules) g
  where
    -- accumulator for used var names
    sld3' :: [VarName] -> Prog -> Goal -> SLDTree
    sld3' _ _ (Goal []) = Node (Goal []) [] -- success node
    sld3' _ (Prog []) goal = Node goal [] -- fail node
    sld3' usedVars (Prog rules') ((Goal (t : ts))) =
      -- inner node
      Node
        (Goal (t : ts))
        ( mapMaybe
            ( \(Rule term termList) -> case unify term t of -- using unify to search for matching rules
                Nothing -> Nothing
                Just su -> Just (su, sld3' (allVars su ++ allVars (Goal (t : ts)) ++ usedVars) (Prog rules') (Goal (map (apply su) (termList ++ ts))))
            )
            renamedRules
        )
      where
        -- rename the programm to avoid name conflicts
        renamedRules = map (rename (usedVars ++ allVars (Goal (t : ts)))) rules
