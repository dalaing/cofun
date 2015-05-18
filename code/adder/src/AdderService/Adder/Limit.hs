module AdderService.Adder.Limit (
    findLimit
  ) where

import           AdderService.Adder        (AdderT, add, clear, total)

import           Control.Monad             (when)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State (StateT, execStateT, modify)

findLimit :: Monad m => AdderT m Int
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

findLimit' :: Monad m => StateT Int (AdderT m) ()
findLimit' = do
  r <- lift $ add 1
  when r $ do
    modify (+ 1)
    findLimit'
