{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveTraversable #-}
module LFPL.AST where

import Data.Functor.Foldable.TH

data LFPLType = 
    LFPLBoolType
  | LFPLIntType
  | LFPLUnitType
  | LFPLDiamondType
  | LFPLFunctionType LFPLType LFPLType 
  | LFPLPairType LFPLType LFPLType 
  | LFPLListType LFPLType
  deriving (Show)

data LFPLTerm identType = 
    LFPLIdentifier identType 
  | LFPLLambda identType LFPLType (LFPLTerm identType) 
  | LFPLApp (LFPLTerm identType) (LFPLTerm identType)
  | LFPLIf (LFPLTerm identType) (LFPLTerm identType) (LFPLTerm identType)
  | LFPLPair (LFPLTerm identType) (LFPLTerm identType)
  -- letp (a, b) = e1 in e2
  | LFPLBindPair identType identType (LFPLTerm identType) (LFPLTerm identType)
  | LFPLListNil
  | LFPLListCons (LFPLTerm identType) (LFPLTerm identType) (LFPLTerm identType) 
  -- | This is iter e { nil -> e0, cons(x1, x2, _) with y -> e1 }
  | LFPLListIter (LFPLTerm identType) (LFPLTerm identType) identType identType identType (LFPLTerm identType)
  | LFPLIntLiteral Integer 
  | LFPLBoolLiteral Bool
  | LFPLUnitLiteral
  | LFPLDiamondLiteral
  | LFPLIntArithOp LFPLIntArithOp (LFPLTerm identType) (LFPLTerm identType)
  | LFPLIntCmpOp LFPLIntCmpOp (LFPLTerm identType) (LFPLTerm identType)

deriving instance Show identType => Show (LFPLTerm identType)

data LFPLIntArithOp = Plus | Minus | Times | Divide | Mod deriving Show 
data LFPLIntCmpOp = 
    LessThan 
  | LessEq
  | GreaterThan
  | GreaterEq
  | Equals deriving Show

makeBaseFunctor ''LFPLTerm
