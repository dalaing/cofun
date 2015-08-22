{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
module Components.Total.Total (
    total
  ) where

import           Components.Total.Functors (TotalF (..))

import           Util.Coproduct            (SumF, Contains(..))

import           Control.Monad.Free        (MonadFree)
import           Control.Monad.Trans.Free  (liftF)

total :: (MonadFree (SumF f) m, Contains TotalF f) => m Int
total = liftF . inj $ Total id
