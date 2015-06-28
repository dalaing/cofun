{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators    #-}
module Components.Add.Add (
    add
  ) where

import           Components.Add.Functors  (AddF (..))

import           Util.Coproduct           ((:<:) (..))

import           Control.Monad.Free (MonadFree)
import           Control.Monad.Trans.Free (liftF)

add :: (MonadFree f m, AddF :<: f) => Int -> m Bool
add x = liftF . inj $ Add x id
