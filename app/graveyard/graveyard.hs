{-

--sld :: Prog -> Goal -> SLDTree
--sld (Prog rules) = sldNew' [] (Prog rules)
--  where
--    sldNew' :: [VarName] -> Prog -> Goal -> SLDTree
--    sldNew' _ (Prog []) goal = Node goal []
--    sldNew' _ _ (Goal []) = Node (Goal []) []
--    sldNew' usedVars (Prog rules) (Goal (t : ts)) = Node (Goal (t : ts)) (mapMaybe (\(Rule term termList) -> (case unify term t of Just s -> Just (s, sldNew' (usedVars ++ allVars (Goal (t : ts)) ++ allVars s) (Prog rules) (Goal (map (apply s) (termList ++ ts)))); Nothing -> Nothing)) renamedRules)
--      where
--        renamedRules = map (rename (usedVars ++ allVars (Goal (t : ts)))) rules

-}
