{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
module Components.Clear.Clear (
    clear
  ) where

import           Components.Clear.Functors (ClearF (..))

import           Util.Coproduct            (Sum, Contains(..), All)

import           Control.Monad.Free (MonadFree)
import           Control.Monad.Trans.Free  (liftF)

clear :: (MonadFree (Sum f) m, All Functor f, Contains ClearF f) => m ()
clear = liftF . inj $ Clear ()
