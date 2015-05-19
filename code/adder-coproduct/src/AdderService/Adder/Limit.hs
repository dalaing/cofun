{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
module AdderService.Adder.Limit (
    findLimit
  ) where

import           Components.Add.Add        (add)
import           Components.Add.Functors   (AddF)
import           Components.Clear.Clear    (clear)
import           Components.Clear.Functors (ClearF)
import           Components.Total.Functors (TotalF)
import           Components.Total.Total    (total)

import           Util.Coproduct            ((:<:) (..))

import           Control.Monad             (when)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Free  (FreeT)
import           Control.Monad.Trans.State (StateT, execStateT, modify)

findLimit :: (Monad m, AddF :<: f, ClearF :<: f, TotalF :<: f) => FreeT f m Int
findLimit = do
   -- capture the old count
   t <- total
   -- clear the count and the state
   clear
   r <- execStateT findLimit' 0
   -- restore the old count
   clear
   _ <- add t
   return r

findLimit' :: (Monad m, AddF :<: f) => StateT Int (FreeT f m) ()
findLimit' = do
  r <- lift $ add 1
  when r $ do
    modify (+ 1)
    findLimit'
