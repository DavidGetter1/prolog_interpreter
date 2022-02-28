import Type

class Pretty a where
  pretty :: a -> String

instance Pretty VarName where
  pretty (VarName name) = name

instance Pretty Term where
  pretty (Var varName) = pretty varName
  pretty (Comb name termList)
    | null termList = name ++ symbolSeperatePretty termList ", "
    | otherwise = name ++ "(" ++ symbolSeperatePretty termList ", " ++ ")"

-- Seperates and returns a list of pretty instances by the symbol
symbolSeperatePretty :: Pretty a => [a] -> String -> String
symbolSeperatePretty [x] _ = pretty x
symbolSeperatePretty (x : xs) s = pretty x ++ s ++ symbolSeperatePretty xs s
symbolSeperatePretty [] s = ""

instance Pretty Rule where
  pretty (Rule term []) = pretty term ++ "."
  pretty (Rule term (x : xs)) = pretty term ++ " :- " ++ symbolSeperatePretty (x : xs) ", " ++ "."

instance Pretty Prog where
  pretty (Prog []) = ""
  pretty (Prog (x : xs)) = symbolSeperatePretty (x : xs) "\n"

instance Pretty Goal where
  pretty (Goal []) = "?- ."
  pretty (Goal (x : xs)) = "?- " ++ symbolSeperatePretty (x : xs) ", " ++ "."

{-

-- Data type for goals
  data Goal = Goal [Term]
  deriving (Show)
    _____

   ghci> pretty (Goal [])
   "?- ."

   ghci> pretty (Goal [Comb "=" [Var (VarName "X"), Comb "false" []]])
   "?- =(X, false)."

   ghci> pretty (Goal [Comb "=" [Var (VarName "X"), Comb "false" []], Comb "=" [Var (VarName "X"), Comb "true" []]])
   "?- =(X, false), =(X, true)."
-}
