{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-} 
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveTraversable #-}
module LFPL.AST where

import Data.List

import Text.Printf

import Data.Functor.Foldable.TH
import Text.Megaparsec.Pos 

type SourceRange = Maybe (SourcePos, SourcePos)
showSourceRange :: SourceRange -> [Char]
showSourceRange Nothing = "<unknown location>"
showSourceRange (Just (start, end)) = 
  printf "%s: %d:%d - %d:%d"
  (sourceName start)
  (unPos $ sourceLine start) (unPos $ sourceColumn start)
  (unPos $ sourceLine end) (unPos $ sourceColumn end)

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
  deriving Show

showLfplType :: LFPLType -> String
showLfplType LFPLBoolType = "bool"
showLfplType LFPLIntType = "int"
showLfplType LFPLUnitType = "unit"
showLfplType LFPLDiamondType = "<>"
showLfplType (LFPLFunctionType a b) = printf "(%s) -> (%s)" (showLfplType a) (showLfplType b)
showLfplType (LFPLPairType a b) = printf "(%s) * (%s)" (showLfplType a) (showLfplType b)
showLfplType (LFPLListType a) = printf "(%s) list" (showLfplType a)
showLfplType (LFPLAnyType) = "<any>"

data LFPLTerm = 
    LFPLIdentifier String 
  | LFPLLambda String LFPLType LFPLTerm 
  | LFPLApp LFPLTerm LFPLTerm
  | LFPLIf LFPLTerm LFPLTerm LFPLTerm
  | LFPLPair LFPLTerm LFPLTerm
  -- letp (a, b) = e1 in e2
  | LFPLBindPair String String LFPLTerm LFPLTerm
  -- | For simplicity and to avoid problems with programs like
  -- fn (x: int) => [], we require the empty list to be annotated
  -- with its type
  | LFPLListNil LFPLType
  | LFPLListCons LFPLTerm LFPLTerm LFPLTerm 
  -- | This is iter e { nil -> e0, cons(x1, x2, _) with y -> e1 }
  | LFPLListIter LFPLTerm LFPLTerm String String String LFPLTerm
  | LFPLIntLiteral Integer 
  | LFPLBoolLiteral Bool
  | LFPLUnitLiteral
  | LFPLDiamondLiteral
  | LFPLIntArithOp LFPLIntArithOp LFPLTerm LFPLTerm
  | LFPLIntCmpOp LFPLIntCmpOp LFPLTerm LFPLTerm
  -- | LFPLPosition startPos term endPos
  | LFPLPositionTerm SourcePos LFPLTerm SourcePos 
  deriving Show


data LFPLIntArithOp = Plus | Minus | Times | Divide | Mod deriving Show 
data LFPLIntCmpOp = 
    LessThan 
  | LessEq
  | GreaterThan
  | GreaterEq
  | Equals
  | NotEquals deriving Show

makeBaseFunctor ''LFPLTerm

showLfplValue :: LFPLTerm -> String
showLfplValue (LFPLIntLiteral i) = show i
showLfplValue (LFPLBoolLiteral b) = show b 
showLfplValue LFPLUnitLiteral = "()"
showLfplValue LFPLDiamondLiteral = "<>"
showLfplValue LFPLListNil {} = "[]"
showLfplValue LFPLLambda {} = "<lambda>"
showLfplValue (t @ LFPLListCons {}) = 
  "[" ++ intercalate ", " (showLfplValue <$> lfplListToList t) ++ "]"
  where lfplListToList (LFPLListNil {}) = []
        lfplListToList (LFPLListCons _ x xs) = x : lfplListToList xs 
        lfplListToList _ = error "showLfplValue: ill-typed expression"
showLfplValue _ = error "showLfplValue: not a value"