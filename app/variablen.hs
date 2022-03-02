module Variablen where

import Data.List (nub)
import Pretty
import Type

-- Returns all VarNames for datatypes: Term, Rule, Prog und Goal
class Vars a where
  allVars :: a -> [VarName]

instance Vars Term where
  allVars (Var varName) = [varName]
  allVars (Comb name termList) = nub (concatMap allVars termList)

instance Vars Rule where
  allVars (Rule rule termList) = nub (allVars rule ++ concatMap allVars termList)

instance Vars Prog where
  allVars (Prog ruleList) = nub (concatMap allVars ruleList)

instance Vars Goal where
  allVars (Goal termList) = nub (concatMap allVars termList)

freshVars :: [VarName]
freshVars = [VarName (i : show j) | j <- [0 ..], i <- ['A' .. 'Z']]

-- case ausdruck
{-

data Term = Var VarName | Comb CombName [Term]
data Rule = Rule Term [Term]
data Prog = Prog [Rule]
data Goal = Goal [Term]

Für den weiteren Verlauf ist es relevant, vorhandene Variablennamen ermitteln sowie frische Variablennamen erzeugen zu können.

    Definieren Sie eine Typklasse Vars, die eine Methode allVars :: a -> [VarName] enthält. Diese Methode soll
     alle in einem Datentyp enthaltenen Variablen (ohne Duplikate) zurückgeben.
     Geben Sie Instanzen dieser Typklasse für die vordefinierten Datentypen Term, Rule, Prog und Goal an.

    Definieren Sie eine Funktion freshVars :: [VarName], die eine unendliche Liste in Prolog gültiger Variablennamen zurückgibt. Diese Liste soll exakt der Form

    [VarName "A",…,VarName "Z",VarName "A0",…,VarName "Z0",VarName "A1",…,VarName "Z1",…]

entsprechen.
-}
