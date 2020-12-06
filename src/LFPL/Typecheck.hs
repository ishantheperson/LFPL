{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module LFPL.Typecheck where

import LFPL.AST
import LFPL.Rename
import LFPL.Error
import LFPL.Util

import Text.Printf
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.List.NonEmpty

import Data.Bifunctor
import Data.Functor.Foldable
import Control.Monad.Reader
import Control.Monad.Writer.Strict

heapFree :: LFPLType -> Bool
heapFree LFPLBoolType = True 
heapFree LFPLIntType = True 
heapFree LFPLUnitType = True 
heapFree LFPLDiamondType = False
heapFree LFPLFunctionType {} = False 
heapFree (LFPLPairType a b) = heapFree a && heapFree b
heapFree LFPLListType {} = False 
heapFree LFPLAnyType = True

typesMatch :: LFPLType -> LFPLType -> Bool
typesMatch LFPLAnyType _ = True 
typesMatch _ LFPLAnyType = True 
typesMatch LFPLBoolType LFPLBoolType = True 
typesMatch LFPLIntType LFPLIntType = True 
typesMatch LFPLUnitType LFPLUnitType = True 
typesMatch LFPLDiamondType LFPLDiamondType = True 
typesMatch (LFPLFunctionType a1 b1) (LFPLFunctionType a2 b2) = 
  typesMatch a1 a2 && typesMatch b1 b2 

typesMatch (LFPLPairType a1 b1) (LFPLPairType a2 b2) = 
  typesMatch a1 a2 && typesMatch b1 b2 

typesMatch (LFPLListType a) (LFPLListType b) = typesMatch a b
typesMatch _ _ = False

data TypeErrorData = 
    -- | TypeMismatch expected actual details
    TypeMismatch LFPLType LFPLType String
  | NotFunction LFPLType
  | NotList LFPLType
  -- | LinearityViolation varName record
  | LinearityViolation String VariableRecord
  deriving Show

data TypeError = TypeError SourceRange TypeErrorData deriving Show

instance CompilerError TypeError where 
  errorMsg (TypeError position err) = 
    let
      errTxt = case err of 
        TypeMismatch expected actual detail -> 
          printf "%s\nexpected: %s\nactual: %s" detail (showLfplType expected) (showLfplType actual)

        NotFunction t -> 
          printf "Type '%s' is not a function" (showLfplType t)

        NotList t -> 
          printf "Type '%s' is not a list" (showLfplType t)
        
        LinearityViolation v VariableRecord{..} -> 
          printf "Variable '%s' of type '%s' is not heap-free and cannot be used multiple times\nLast usage: %s\nDeclared at: %s" 
                  (unmangle v) 
                  (showLfplType variableType) 
                  (showSourceRange variableUsagePosition)
                  (showSourceRange variableDeclarationPosition)
    in 
      showSourceRange position ++ ": " ++ errTxt


data VariableRecord = VariableRecord
  {
    variableType :: LFPLType,
    variableUsed :: Bool,
    variableDeclarationPosition :: SourceRange,
    variableUsagePosition :: SourceRange
  } deriving Show

type VariableContext = Map String VariableRecord
type TypecheckContext = (SourceRange, VariableContext)
type Typechecker m = 
  (MonadReader TypecheckContext m, MonadWriter (Maybe (NonEmpty TypeError)) m)

typecheck :: LFPLTerm -> Either (NonEmpty TypeError) LFPLType
typecheck = runTypechecker . fold go
  where go :: Typechecker m => LFPLTermF (m (LFPLType, VariableContext)) -> m (LFPLType, VariableContext)
        go (LFPLIdentifierF v) = do 
          record@VariableRecord{..} <- lookupVar v
          position <- getPosition

          -- Check if its okay to use this variable
          if variableUsed && not (heapFree variableType)
            then do 
              tellError (TypeError position (LinearityViolation v record))
              return (variableType, Map.singleton v record)
            else 
              -- Update the variable record with the usage information
              return (variableType, 
                      Map.singleton v (record { variableUsed = True, variableUsagePosition = position }))


        go (LFPLLambdaF paramName paramType body) = do 
          position <- getPosition 
          let record = VariableRecord { variableType = paramType,
                                        variableUsed = False,
                                        variableDeclarationPosition = position,
                                        variableUsagePosition = Nothing }
          (bodyType, context) <- local (second $ Map.insert paramName record) body
          return (LFPLFunctionType paramType bodyType, context)

        go (LFPLAppF e1 e2) = do
          position <- getPosition

          (e1Type, e1Context) <- e1
          -- Perform updates from e1
          context <- Map.union e1Context <$> getContext

          case e1Type of
            LFPLFunctionType t1 t2 -> do 
              -- e2 must have type t1 
              (e2Type, e2Context) <- local (second (const context)) e2
              unless (typesMatch t1 e2Type) $
                tellError (TypeError position (
                  TypeMismatch t1 e2Type "function argument is the wrong type"))

              -- Perform updates from e2
              return (t2, Map.union e2Context context)

            -- Propagate type error
            LFPLAnyType -> return (LFPLAnyType, context)
            _ -> do
              tellError $ TypeError position (NotFunction e1Type)
              return (LFPLAnyType, context)

        go (LFPLIntArithOpF _ e1 e2) = do 
          position <- getPosition
          (e1Type, e1Context) <- e1 
          context <- Map.union e1Context <$> getContext

          (e2Type, e2Context) <- local (second (const context)) e2

          unless (typesMatch e1Type LFPLIntType) $
            tellError (TypeError position (
              TypeMismatch LFPLIntType e1Type "first operand is not an int"))
          
          unless (typesMatch e2Type LFPLIntType) $
            tellError (TypeError position (
              TypeMismatch LFPLIntType e2Type "second operand is not an int"))

          return (LFPLIntType, Map.union e2Context context)

        go (LFPLIntCmpOpF _ e1 e2) = do 
          position <- getPosition
          (e1Type, e1Context) <- e1 
          context <- Map.union e1Context <$> getContext

          (e2Type, e2Context) <- local (second (const context)) e2

          unless (typesMatch e1Type LFPLIntType) $
            tellError (TypeError position (
              TypeMismatch LFPLIntType e1Type "first operand is not an int"))
          
          unless (typesMatch e2Type LFPLIntType) $
            tellError (TypeError position (
              TypeMismatch LFPLIntType e2Type "second operand is not an int"))

          return (LFPLBoolType, Map.union e2Context context)


        go (LFPLListNilF t) = return (LFPLListType t, Map.empty)
        go (LFPLListConsF diamond listHead listTail) = do 
          position <- getPosition
          
          (diamondType, diamondContext) <- diamond 
          context <- Map.union diamondContext <$> getContext

          (listHeadType, listHeadContext) <- local (second (const context)) listHead
          context <- return $ Map.union listHeadContext context 

          (listTailType, listTailContext) <- local (second (const context)) listTail
          context <- return $ Map.union listTailContext context 

          unless (typesMatch diamondType LFPLDiamondType) $
            tellError (TypeError position (
              TypeMismatch LFPLDiamondType diamondType "first argument to cons should be a diamond"
            ))

          case listTailType of 
            LFPLListType elemType -> do 
              unless (typesMatch elemType listHeadType) $ 
                tellError (TypeError position (
                  TypeMismatch listHeadType elemType "list tail elements are not the same type as the head element"
                ))

              return (LFPLListType elemType, context)

            LFPLAnyType -> return (LFPLListType LFPLAnyType, context)
            _ -> do 
              tellError (TypeError position (NotList listTailType))
              return (LFPLListType LFPLAnyType, context)

        go (LFPLListIterF list nilCase diamond headItem recursiveResult consCase) = do 
          position <- getPosition

          (listType, listContext) <- list 
          context <- Map.union listContext <$> getContext 

          listElemType <- case listType of 
            LFPLListType elemType -> return elemType 
            LFPLAnyType -> return LFPLAnyType 
            _ -> do 
              tellError (TypeError position (NotList listType))
              return LFPLAnyType

          -- Typecheck nilCase.
          -- Note context is only split between list and nilCase
          -- not consCase
          (nilCaseType, nilCaseContext) <- local (second (const context)) nilCase

          -- Create records for consCase
          let mkRecord t = VariableRecord { variableType = t,
                                            variableUsed = False,
                                            variableDeclarationPosition = position,
                                            variableUsagePosition = Nothing }
              diamondRecord = mkRecord LFPLDiamondType
              headItemRecord = mkRecord listElemType
              recursiveResultRecord = mkRecord nilCaseType

              consContext = Map.fromList [(diamond, diamondRecord),
                                          (headItem, headItemRecord),
                                          (recursiveResult, recursiveResultRecord)]

          (consCaseType, consCaseContext) <- local (second (const consContext)) consCase

          unless (typesMatch nilCaseType consCaseType) $
            tellError (TypeError position (
              TypeMismatch nilCaseType consCaseType "nil and cons cases in iter don't match"
            ))

          return (consCaseType, Map.unions [consCaseContext, nilCaseContext, context])

        go (LFPLIfF e1 e2 e3) = do 
          position <- getPosition 

          (testType, testContext) <- e1 
          context <- Map.union testContext <$> getContext 

          (trueType, trueContext) <- local (second (const context)) e2 
          (falseType, falseContext) <- local (second (const context)) e3

          unless (typesMatch testType LFPLBoolType) $
            tellError (TypeError position (
              TypeMismatch LFPLBoolType testType "if expression requires a boolean test"
            ))

          unless (typesMatch trueType falseType) $
            tellError (TypeError position (
              TypeMismatch trueType falseType "branches in if expression don't match"
            ))

          return (trueType, Map.unions [falseContext, trueContext, context])

        go (LFPLIntLiteralF _) = return (LFPLIntType, Map.empty)
        go (LFPLBoolLiteralF _) = return (LFPLBoolType, Map.empty)
        go LFPLUnitLiteralF = return (LFPLUnitType, Map.empty)
        go LFPLDiamondLiteralF = return (LFPLDiamondType, Map.empty)
        go (LFPLPositionTermF startPos term endPos) = 
          local (first (const $ Just (startPos, endPos))) term

lookupVar :: Typechecker m => String -> m VariableRecord
lookupVar v = do 
  maybeLookup <- asks (Map.lookup v . snd)
  case maybeLookup of 
    Nothing -> error $ "typecheck: All variables should be bound " ++ v
    Just record -> return record

getPosition :: Typechecker m => m (SourceRange)
getPosition = asks fst

getContext :: Typechecker m => m VariableContext
getContext = asks snd

runTypechecker f =
  case runWriter (runReaderT f (Nothing, Map.empty)) of 
    ((t, _), Nothing) -> Right t
    (_, Just errs) -> Left errs