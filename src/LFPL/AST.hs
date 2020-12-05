{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-} 
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveTraversable #-}
module LFPL.AST where

import Data.Functor.Foldable.TH
import Text.Megaparsec.Pos 

type SourceRange = (SourcePos, SourcePos)

data LFPLType = 
    LFPLBoolType
  | LFPLIntType
  | LFPLUnitType
  | LFPLDiamondType
  | LFPLFunctionType LFPLType LFPLType 
  | LFPLPairType LFPLType LFPLType 
  | LFPLListType LFPLType
  -- | Not a 'real LFPL type', but used when
  -- a typing error occurs
  | LFPLAnyType
  deriving (Show)

data LFPLTerm identType = 
    LFPLIdentifier identType 
  | LFPLLambda identType LFPLType (LFPLTerm identType) 
  | LFPLApp (LFPLTerm identType) (LFPLTerm identType)
  | LFPLIf (LFPLTerm identType) (LFPLTerm identType) (LFPLTerm identType)
  | LFPLPair (LFPLTerm identType) (LFPLTerm identType)
  -- letp (a, b) = e1 in e2
  | LFPLBindPair identType identType (LFPLTerm identType) (LFPLTerm identType)
  -- | For simplicity and to avoid problems with programs like
  -- fn (x: int) => [], we require the empty list to be annotated
  -- with its type
  | LFPLListNil LFPLType
  | LFPLListCons (LFPLTerm identType) (LFPLTerm identType) (LFPLTerm identType) 
  -- | This is iter e { nil -> e0, cons(x1, x2, _) with y -> e1 }
  | LFPLListIter (LFPLTerm identType) (LFPLTerm identType) identType identType identType (LFPLTerm identType)
  | LFPLIntLiteral Integer 
  | LFPLBoolLiteral Bool
  | LFPLUnitLiteral
  | LFPLDiamondLiteral
  | LFPLIntArithOp LFPLIntArithOp (LFPLTerm identType) (LFPLTerm identType)
  | LFPLIntCmpOp LFPLIntCmpOp (LFPLTerm identType) (LFPLTerm identType)
  -- | LFPLPosition startPos term endPos
  | LFPLPositionTerm SourcePos (LFPLTerm identType) SourcePos 

deriving instance Show identType => Show (LFPLTerm identType)

data LFPLIntArithOp = Plus | Minus | Times | Divide | Mod deriving Show 
data LFPLIntCmpOp = 
    LessThan 
  | LessEq
  | GreaterThan
  | GreaterEq
  | Equals
  | NotEquals deriving Show

makeBaseFunctor ''LFPLTerm
