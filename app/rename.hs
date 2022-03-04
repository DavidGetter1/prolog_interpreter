module Rename where

import Data.List (nub)
import Pretty
import Substitutionen
import Test.QuickCheck
import Type
import Unifikation
import Variablen

-- a(auto) :- b(baum), u(uboot)

--

-- renames all variables in the passed rule, that are elements of the passed list
rename :: [VarName] -> Rule -> Rule
rename varList (Rule term termList) = Rule (apply renameSubst term) (map (apply renameSubst) termList)
  where
    renameSubst = Subst (zip varList (take (length varList) (map Var freshVars)))
