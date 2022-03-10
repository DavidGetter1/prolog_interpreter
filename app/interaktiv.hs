module Interaktiv where

import Data.List
import Parser (parse, parseFile)
import Pretty
import SLD
import Substitutionen (Subst)
import Type

-- shows a long string, does not belong into main program because too long
showHelpString :: IO ()
showHelpString =
  putStrLn ("Commands available from the prompt\n <goal>     Solves/proves the specified goal.\n:h          Shows this help message.\n:l <file>   Loads the specified file.\n:p          Prints the currently loaded program.\n:q          Exits the interactive environment.\n:r          Reloads the last loaded file.\n:s <strat>  Sets the specified search strategy\n            where <strat> is one of 'dfs', 'bfs', or 'iddfs'.\n:t <goal>   Prints the SLD tree for the specified goal.")

--quits the program
quit :: IO ()
quit = putStrLn ""

-- REPR loop
prolog :: IO ()
prolog = do
  putStrLn "Welcome!"
  putStrLn "Type ':h' for help."
  prolog' "" dfs (Prog [])
  where
    prolog' :: String -> Strategy -> Prog -> IO ()
    prolog' progSource strat p = do
      putStr ":-"
      c <- getLine
      case c of
        ":q" -> quit
        ":r" ->
          putStrLn progSource
            >> parseFile progSource >>= \parsedProg -> case parsedProg of
              Left s -> putStrLn s >> prolog' progSource strat p
              Right prog -> putStrLn "Program Loaded." >> prolog' progSource strat prog
        ":h" -> showHelpString >> prolog' progSource strat p
        ":p" -> case p of Prog [] -> putStrLn "No Program Loaded." >> prolog' progSource strat p; _ -> putStrLn (pretty p) >> prolog' progSource strat p
        _ ->
          if ":l" `isPrefixOf` c
            then
              parseFile (tail (tail (tail c))) >>= \x -> case x of
                Left s -> putStrLn s >> prolog' progSource strat p
                Right prog -> putStrLn "Program Loaded." >> prolog' (tail (tail (tail c))) strat prog
            else
              ( if ":t" `isPrefixOf` c
                  then case parse (tail (tail (tail c))) of
                    Left s -> putStrLn s >> prolog' progSource strat p
                    Right goal -> putStrLn (show (sld p goal)) >> prolog' progSource strat p
                  else
                    ( if ":s" `isPrefixOf` c
                        then case tail (tail (tail c)) of
                          "dfs" -> putStrLn "Strat successfully changed to dfs" >> prolog' progSource dfs p
                          "bfs" -> putStrLn "Strat successfully changed to bfs" >> prolog' progSource bfs p
                          _ -> putStrLn "Strat not recognized" >> prolog' progSource bfs p
                        else case parse c :: Either String Goal of
                          Left s -> putStrLn s >> prolog' progSource dfs p
                          Right goal -> resultPrinting strat p (solveWith p goal strat)
                    )
              )
      where
        -- printing results bit by bit
        resultPrinting :: Strategy -> Prog -> [Subst] -> IO ()
        resultPrinting strategy' prog [] = prolog' progSource strategy' prog
        resultPrinting strategy prog (x : xs) = do
          putStrLn (pretty x)
          c <- getLine --getChar schluckt ne lösung!?
          if c == ";"
            then resultPrinting strategy prog xs -- show next result
            else resultPrinting strategy prog [] -- continue program without showing possible remaining solutions