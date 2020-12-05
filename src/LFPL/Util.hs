{-# LANGUAGE FlexibleContexts #-}
module LFPL.Util where

import Control.Monad.Writer.Strict
import Data.List.NonEmpty

tellError :: MonadWriter (Maybe (NonEmpty a)) m => a -> m ()
tellError = tell . Just . pure