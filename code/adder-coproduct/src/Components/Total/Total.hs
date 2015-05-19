{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
module Components.Total.Total (
    total
  ) where

import           Components.Total.Functors (TotalF (..))

import           Util.Coproduct            ((:<:) (..))

import           Control.Monad.Trans.Free  (FreeT, liftF)

total :: (Functor f, Monad m, TotalF :<: f) => FreeT f m Int
total = liftF . inj $ Total id
