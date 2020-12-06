module Main where

import LFPL.AST
import LFPL.Compile

import System.Environment
import System.Exit

import Text.Show.Pretty 
import Language.Haskell.HsColour
import Language.Haskell.HsColour.Colourise

colorPrint :: Show a => a -> IO () 
colorPrint = putStrLn . hscolour TTY defaultColourPrefs False False "" False . ppShow 

runStr :: String -> IO ()
runStr txt = do 
  case compile "<stdin>" txt Nothing of 
    Left err -> do { putStrLn (unpackError err); exitFailure }
    Right (t, result) -> do { putStrLn (showLfplValue result ++ ": " ++ showLfplType t); exitSuccess }

runFile :: FilePath -> Maybe String -> IO ()
runFile fname arg = do 
  txt <- readFile fname 
  case compile fname txt arg of 
    Left err -> do { putStrLn (unpackError err); exitFailure }
    Right (t, result) -> do { putStrLn (showLfplValue result ++ ": " ++ showLfplType t); exitSuccess }

usage :: IO ()
usage = do 
  mapM_ putStrLn 
    [ "usage: lfpl <file> [input]"
    , ""
    , "Executes the file, printing out the type and result." 
    , "Optionally, input can be provided, given that file contains"
    , "a valid LFPL function of the right type"
    , ""
    , "When specifying input, lists can be created using"
    , "the usual syntactic sugar e.g. ([1, 2, 3, 4]: int)."
    , "Note that you must also add the type of the elements (not the list)"
    , "This is the only way of getting diamonds into the program"
    ]

main :: IO ()
main = do
  args <- getArgs
  case args of 
    [] -> usage
    [fname] -> runFile fname Nothing-- eval
    [fname, arg] -> runFile fname (Just arg)
    _ -> usage