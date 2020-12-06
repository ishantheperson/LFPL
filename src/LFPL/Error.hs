module LFPL.Error where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty

class CompilerError e where
  errorMsg :: e -> String

instance CompilerError e => CompilerError (NonEmpty e) where 
  errorMsg = concat . NonEmpty.toList . NonEmpty.intersperse "\n" . fmap errorMsg
