module AdderService.Adder.Interpreter.Iter (
    interpret
  ) where

import           AdderService.Adder        (AdderT)
import           AdderService.Functors     (AdderF (..))

import           Control.Monad.Trans.Free  (FreeF (..), FreeT (..))

import           Control.Monad.Reader      (ReaderT, ask, runReaderT)
import           Control.Monad.State       (StateT, evalStateT, get, put)
import           Control.Monad.Trans.Class (MonadTrans, lift)

type Limit = Int
type Count = Int

type Base m = ReaderT Limit (StateT Count m)

step :: Monad m => AdderF (Base m r) -> Base m r
step (Add x k) = do
  limit <- ask
  count <- lift get
  let count' = x + count
  let test = count' <= limit
  let next = if test then count' else count
  lift . put $ next
  k test
step (Clear k) = do
  lift . put $ 0
  k
step (Total k) = do
  t <- lift get
  k t

iterTTM :: (Functor f, Monad m, MonadTrans t1, MonadTrans t2, Monad (t1 (t2 m)), Monad (t2 m)) => (f (t1 (t2 m) a) -> t1 (t2 m) a) -> FreeT f m a -> t1 (t2 m) a
iterTTM f (FreeT m) = do
    val <- lift . lift $ m
    case fmap (iterTTM f) val of
        Pure x -> return x
        Free y -> f y

interpret :: Monad m => Int -> Int -> AdderT m r -> m r
interpret limit count = flip evalStateT count .
                         flip runReaderT limit .
                         iterTTM step
