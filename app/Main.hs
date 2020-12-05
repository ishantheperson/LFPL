module Main where

import LFPL.Parser
import LFPL.Rename
import LFPL.Typecheck
import LFPL.Eval

import Text.Show.Pretty 
import Language.Haskell.HsColour
import Language.Haskell.HsColour.Colourise

colorPrint :: Show a => a -> IO () 
colorPrint = putStrLn . hscolour TTY defaultColourPrefs False False "" False . ppShow 

runStr s = do 
  let parsed = run s 
      Right renamed = rename parsed 
  
  t <- case typecheck renamed of
            Left errs -> do { print errs; error "done" }
            Right t -> return t 

  colorPrint t
  let result = eval renamed 
  
  colorPrint result

runFile s = readFile s >>= runStr

main :: IO ()
main = runFile "test/isort.lfpl"