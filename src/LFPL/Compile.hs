{-# LANGUAGE ExistentialQuantification #-}
module LFPL.Compile where

import LFPL.AST
import LFPL.Error

import LFPL.Parser
import LFPL.Rename
import LFPL.Typecheck
import LFPL.Eval

data PackedCompilerError = forall e. CompilerError e => PackedCompilerError e
instance Show PackedCompilerError where 
  show = unpackError

type CompilerM a = Either PackedCompilerError a

liftCompiler :: CompilerError e => Either e a -> CompilerM a 
liftCompiler = either (Left . PackedCompilerError) Right

unpackError :: PackedCompilerError -> String 
unpackError (PackedCompilerError e) = errorMsg e

-- We return the type of the expression as well as the output value
type LFPLOutput = (LFPLType, LFPLTerm)

compile :: FilePath -> String -> Maybe String -> Either PackedCompilerError LFPLOutput
compile fname str maybeArg = do 
  rawAst <- liftCompiler $ parseTerm (ParserConfig False) fname str
  rawArg <- case maybeArg of 
              Nothing -> return Nothing
              Just argStr -> Just <$> liftCompiler (parseTerm (ParserConfig True) "<input>" argStr)
  
  rawAst <- return $ case rawArg of 
              Nothing -> rawAst
              Just arg -> LFPLApp rawAst arg 

  ast <- liftCompiler $ rename rawAst
  programType <- liftCompiler $ typecheck ast 

  return (programType, eval ast)