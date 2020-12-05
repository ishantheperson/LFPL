{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module LFPL.Eval where 

import LFPL.AST 

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe

import Data.Functor.Foldable

import Control.Monad.Reader

-- data LFPLValue = 
--     LFPLBoolValue Bool
--   | LFPLIntValue Integer 
--   | LFPLUnitValue
--   | LFPLDiamondValue
--   | LFPLFunctionValue EvalContext String (LFPLTerm String)
--   | LFPLPairValue (LFPLValue, LFPLValue)
--   | LFPLListValue [LFPLValue]

-- type EvalContext = Map String LFPLValue
-- type Evaluator = Reader EvalContext

-- eval :: LFPLTerm String -> LFPLValue
-- eval = runEvaluator . cata go 
--   where go :: LFPLTermF String (Evaluator LFPLValue) -> Evaluator LFPLValue
--         go (LFPLIdentifierF v) = asks (fromJust . Map.lookup v)
--         go (LFPLLambdaF param _ body) = do
--           context <- ask
--           return $ LFPLFunctionValue context param body
--           -- return $ LFPLFunctionValue (\x -> local (Map.insert param x) body)


eval :: LFPLTerm String -> LFPLTerm String 
eval (LFPLIdentifier _) = error "eval: Term should not contain free variables"
eval (LFPLApp e1 e2) = 
  case eval e1 of 
    LFPLLambda paramName _ body -> eval (subst paramName (eval e2) body)
    _ -> error "eval: Expected lambda for application"

eval (LFPLIf e1 e2 e3) = 
  case eval e1 of 
    LFPLBoolLiteral True -> eval e2 
    LFPLBoolLiteral False -> eval e3 
    _ -> error "eval: Expected bool for if"

eval (LFPLListIter list nilCase diamond headItem recursiveResult consCase) =
  lfplIter 
    nilCase
    (\x y z -> subst diamond x $ subst headItem y $ subst recursiveResult z $ consCase)
    (eval list)

eval (LFPLIntCmpOp op e1 e2) = 
  let func = case op of 
               LessThan -> (<)
               LessEq -> (<=)
               GreaterThan -> (>)
               GreaterEq -> (>=)
               Equals -> (==)
  in LFPLBoolLiteral (func (evalInt e1) (evalInt e2))

eval (LFPLIntArithOp op e1 e2) = 
  let func = case op of 
               Plus -> (+)
               Minus -> (-)
               Times -> (*)
               Divide -> div
               Mod -> mod

  in LFPLIntLiteral (func (evalInt e1) (evalInt e2))

-- Maybe values
eval (LFPLListCons diamond listHead listTail) =
  LFPLListCons (eval diamond) (eval listHead) (eval listTail)

-- Definitely values
eval (t @ LFPLListNil) = t
eval (t @ (LFPLLambda {})) = t
eval (t @ (LFPLIntLiteral {})) = t
eval (t @ (LFPLBoolLiteral {})) = t
eval (t @ LFPLUnitLiteral) = t
eval (t @ LFPLDiamondLiteral) = t

-- lfplIter :: a -> (LFPLTerm String -> a -> a) -> LFPLTerm String -> a
lfplIter nilCase consCase = eval . \case 
  LFPLListNil -> nilCase 
  LFPLListCons diamond listHead listTail -> 
    consCase diamond listHead (lfplIter nilCase consCase listTail)

evalInt :: LFPLTerm String -> Integer
evalInt t = case eval t of 
  LFPLIntLiteral i -> i
  _ -> error "eval: Expected integer"

-- subst x v e = [v/x]e
subst :: String -> LFPLTerm String -> LFPLTerm String -> LFPLTerm String 
subst x v = cata go 
  where go :: LFPLTermF String (LFPLTerm String) -> LFPLTerm String
        go (LFPLIdentifierF y) | x == y = v
                               | otherwise = LFPLIdentifier y
        go other = embed other
