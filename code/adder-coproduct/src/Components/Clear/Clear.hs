{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
module Components.Clear.Clear (
    clear
  ) where

import           Components.Clear.Functors (ClearF (..))

import           Util.Coproduct            ((:<:) (..))

import           Control.Monad.Trans.Free  (FreeT, liftF)

clear :: (Functor f, Monad m, ClearF :<: f) => FreeT f m ()
clear = liftF . inj $ Clear ()
