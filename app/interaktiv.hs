module Interaktiv where

import Data.List
import Parser (parse, parseFile)
import SLD
import System.Exit
import Type

showHelpString :: IO ()
showHelpString = do
  putStrLn ("Commands available from the prompt\n <goal>     Solves/proves the specified goal.\n:h          Shows this help message.\n:l <file>   Loads the specified file.\n:p          Prints the currently loaded program.\n:q          Exits the interactive environment.\n:r          Reloads the last loaded file.\n:s <strat>  Sets the specified search strategy\n            where <strat> is one of 'dfs', 'bfs', or 'iddfs'.\n:t <goal>   Prints the SLD tree for the specified goal.")

quit :: IO ()
quit = putStrLn ("")

prolog :: IO ()
prolog = do
  putStrLn ("Welcome!")
  putStrLn ("Type ':h' for help.")
  prolog' (Prog [])
  where
    prolog' :: Prog -> IO ()
    prolog' p = do
      putStr (":-")
      c <- getLine
      case c of
        ":q" -> quit
        ":h" -> showHelpString >>= \x -> prolog' p
        ":p" -> case p of Prog [] -> putStrLn ("No Program Loaded.") >>= \x -> prolog' p; _ -> putStrLn (show p) >>= \x -> prolog' p
        _ ->
          if ":l" `isPrefixOf` c
            then
              parseFile (tail (tail (tail c))) >>= \x -> case x of
                Left s -> putStrLn ("No program loaded.") >>= \x -> prolog' p
                Right any -> putStrLn ("Program Loaded.") >>= \x -> prolog' any
            else
              ( if ":t" `isPrefixOf` c
                  then case parse (tail (tail (tail c))) of
                    Left s -> putStrLn ("Goal could not be loaded.") >>= \x -> prolog' p
                    Right any -> putStrLn (show (sld p any)) >>= \x -> prolog' p
                  else prolog
              )