{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
module Components.Clear.Clear (
    clear
  ) where

import           Components.Clear.Functors (ClearF (..))

import           Util.Coproduct            (SumF, Contains(..))

import           Control.Monad.Free (MonadFree)
import           Control.Monad.Trans.Free  (liftF)

clear :: (MonadFree (SumF f) m, Contains ClearF f) => m ()
clear = liftF . inj $ Clear ()
