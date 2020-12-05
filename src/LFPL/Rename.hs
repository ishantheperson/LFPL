{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLists #-}
module LFPL.Rename where

import LFPL.AST 

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.List.NonEmpty (NonEmpty(..))

import Data.Functor.Foldable
import Control.Monad.State.Strict
import Control.Monad.State.Class
import Control.Monad.Reader.Class
import Control.Monad.Reader
import Control.Monad.Writer.Class
import Control.Monad.Writer.Strict

data RenameError = 
    UnknownVariable String
    deriving Show

-- We store a mapping from
-- a source variable name to its renamed/mangled version
type RenameMapping = Map String String
-- While renaming, we need to keep track of 
-- our rename mapping, any errors produced,
-- as well as the next available # for the 
type Renamer m = 
    (MonadReader RenameMapping m, MonadWriter (Maybe (NonEmpty RenameError)) m, MonadState Int m)

-- rename :: forall m. Renamer m => LFPLTerm String -> m (LFPLTerm String)
rename :: LFPLTerm String -> Either (NonEmpty RenameError) (LFPLTerm String)
rename = runRenamer . fold go 
  where go :: Renamer m => LFPLTermF String (m (LFPLTerm String)) -> m (LFPLTerm String)
        go (LFPLIdentifierF v) = do
          maybeMappedName <- asks (Map.lookup v)
          case maybeMappedName of
            Nothing -> do 
              tell . Just . pure $ UnknownVariable v
              return $ LFPLIdentifier v
            Just realName -> return $ LFPLIdentifier realName

        go (LFPLLambdaF paramName paramType body) = do
          renamedParam <- mangle paramName
          renamedBody <- local (Map.insert paramName renamedParam) body
          return $ LFPLLambda renamedParam paramType renamedBody

        -- TODO: in cases with multiple binders, should make sure
        -- the same name isn't bound twice
        go (LFPLBindPairF leftName rightName pairExp body) = do
          renamedLeft <- mangle leftName 
          renamedRight <- mangle rightName
          renamedPairExp <- pairExp
          renamedBody <- local (Map.union [(leftName, renamedLeft), (rightName, renamedRight)]) body 
          return $ LFPLBindPair renamedLeft renamedRight renamedPairExp renamedBody

        go (LFPLListIterF list nilCase diamond headItem recursiveResult consCase) = do 
          renamedList <- list 
          renamedNilCase <- nilCase 
          renamedDiamond <- mangle diamond 
          renamedHeadItem <- mangle headItem 
          renamedRecursiveResult <- mangle recursiveResult 

          renamedConsCase <- local (Map.union [
                                    (diamond, renamedDiamond), 
                                    (headItem, renamedHeadItem), 
                                    (recursiveResult, renamedRecursiveResult)
                                   ]) 
                                  consCase

          return $ LFPLListIter renamedList renamedNilCase 
                                renamedDiamond renamedHeadItem renamedRecursiveResult renamedConsCase

        go nonBindingTerm = embed <$> sequence nonBindingTerm

mangle :: forall m. MonadState Int m => String -> m String
mangle str = do
  nextNumber <- get 
  modify' succ 
  return $ str ++ "_" ++ show nextNumber

runRenamer :: ReaderT RenameMapping (StateT Int (Writer (Maybe (NonEmpty RenameError)))) a -> Either (NonEmpty RenameError) a
runRenamer f = 
  case runWriter (evalStateT (runReaderT f Map.empty) 0) of 
    (renamed, Nothing) -> Right renamed 
    (_, Just errors) -> Left errors
