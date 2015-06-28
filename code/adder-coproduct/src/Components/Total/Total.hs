{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
module Components.Total.Total (
    total
  ) where

import           Components.Total.Functors (TotalF (..))

import           Util.Coproduct            ((:<:) (..))

import           Control.Monad.Free        (MonadFree)
import           Control.Monad.Trans.Free  (liftF)

total :: (MonadFree f m, TotalF :<: f) => m Int
total = liftF . inj $ Total id
