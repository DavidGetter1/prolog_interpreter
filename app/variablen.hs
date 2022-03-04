module Variablen where

import Data.List (nub)
import Type

-- Returns all VarNames for datatypes: Term, Rule, Prog und Goal
class Vars a where
  allVars :: a -> [VarName]

instance Vars Term where
  allVars (Var varName) = [varName]
  allVars (Comb _ termList) = nub (concatMap allVars termList)

instance Vars Rule where
  allVars (Rule rule termList) = nub (allVars rule ++ concatMap allVars termList)

instance Vars Prog where
  allVars (Prog ruleList) = nub (concatMap allVars ruleList)

instance Vars Goal where
  allVars (Goal termList) = nub (concatMap allVars termList)

freshVars :: [VarName]
freshVars = map (VarName . (: "")) ['A' .. 'Z'] ++ [VarName (i : show j) | j <- [0 :: Integer ..], i <- ['A' .. 'Z']]
