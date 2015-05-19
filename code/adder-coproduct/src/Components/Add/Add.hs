{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
module Components.Add.Add (
    add
  ) where

import           Components.Add.Functors  (AddF (..))

import           Util.Coproduct           ((:<:) (..))

import           Control.Monad.Trans.Free (FreeT, liftF)

add :: (Functor f, Monad m, AddF :<: f) => Int -> FreeT f m Bool
add x = liftF . inj $ Add x id
