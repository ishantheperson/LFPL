module Main where

import LFPL.AST
import LFPL.Parser
import LFPL.Rename
import LFPL.Typecheck
import LFPL.Eval
import LFPL.Compile

import System.Environment

import Text.Show.Pretty 
import Language.Haskell.HsColour
import Language.Haskell.HsColour.Colourise

colorPrint :: Show a => a -> IO () 
colorPrint = putStrLn . hscolour TTY defaultColourPrefs False False "" False . ppShow 

runFile :: FilePath -> IO ()
runFile fname = do 
  txt <- readFile fname 
  putStrLn $ case compile fname txt of 
    Left err -> unpackError err
    Right (t, result) -> showLfplValue result ++ ": " ++ showLfplType t

usage :: IO ()
usage = do 
  mapM_ putStrLn 
    [ "usage: lfpl [file] [input]"
    , ""
    , "If file is blank, then the REPL is launched"
    , "Otherwise, executes the file, printing out the type and result." 
    , "Optionally, input can be provided, given that file contains"
    , "a valid LFPL function of the right type"
    , ""
    , "When specifying input, lists can be created using"
    , "the usual syntactic sugar e.g. [1, 2, 3, 4]."
    , "This is the only way of getting diamonds into the program"
    ]

main :: IO ()
main = do
  args <- getArgs
  case args of 
    [] -> usage -- launch REPL?
    [fname] -> runFile fname-- eval 
    _ -> usage