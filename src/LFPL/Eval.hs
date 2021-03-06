{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module LFPL.Eval where 

import LFPL.AST 

import Data.Functor.Foldable


eval :: LFPLTerm -> LFPLTerm 
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

eval (LFPLBindPair e1Name e2Name pair body) = 
  case eval pair of 
    LFPLPair e1 e2 -> eval (subst e1Name e1 $ subst e2Name e2 body)
    _ -> error "eval: Expected pair"

eval (LFPLIntCmpOp op e1 e2) = 
  let func = case op of 
               LessThan -> (<)
               LessEq -> (<=)
               GreaterThan -> (>)
               GreaterEq -> (>=)
               Equals -> (==)
               NotEquals -> (/=)
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

eval (LFPLPair e1 e2) = 
  LFPLPair (eval e1) (eval e2)

-- Definitely values
eval (t @ LFPLListNil {}) = t
eval (t @ (LFPLLambda {})) = t
eval (t @ (LFPLIntLiteral {})) = t
eval (t @ (LFPLBoolLiteral {})) = t
eval (t @ LFPLUnitLiteral) = t
eval (t @ LFPLDiamondLiteral) = t

eval (LFPLPositionTerm _ t _) = eval t 

-- | lfplIter nilCase consCase L = Runs the iterator on the given list.
-- The 'nilCase' term will be evaluated when L is nil.
-- For the cons case, the function 'consCase' should take in 3 arguments
-- (the diamond, head, and recursive result) and should produce a term
-- with those values substituted in
lfplIter :: LFPLTerm -> (LFPLTerm -> LFPLTerm -> LFPLTerm -> LFPLTerm) -> LFPLTerm -> LFPLTerm
lfplIter nilCase consCase = eval . \case 
  LFPLListNil {} -> nilCase 
  LFPLListCons diamond listHead listTail -> 
    consCase diamond listHead (lfplIter nilCase consCase listTail)
  _ -> error "lfplIter: ill-typed expression"

evalInt :: LFPLTerm -> Integer
evalInt t = case eval t of 
  LFPLIntLiteral i -> i
  _ -> error "eval: Expected integer"

-- subst x v e = [v/x]e
subst :: String -> LFPLTerm -> LFPLTerm -> LFPLTerm 
subst x v = para go 
  where go :: LFPLTermF (LFPLTerm, LFPLTerm) -> LFPLTerm
        go (LFPLIdentifierF y) | x == y = v
                               | otherwise = LFPLIdentifier y

        go (LFPLLambdaF y t (oldBody, replacedBody)) | x == y = LFPLLambda y t oldBody 
                                                     | otherwise = LFPLLambda y t replacedBody
        go other = embed (snd <$> other)
