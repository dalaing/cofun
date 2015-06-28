{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
module Components.Clear.Clear (
    clear
  ) where

import           Components.Clear.Functors (ClearF (..))

import           Util.Coproduct            ((:<:) (..))

import           Control.Monad.Free (MonadFree)
import           Control.Monad.Trans.Free  (liftF)

clear :: (MonadFree f m, ClearF :<: f) => m ()
clear = liftF . inj $ Clear ()
