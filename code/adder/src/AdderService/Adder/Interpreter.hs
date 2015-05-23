module AdderService.Adder.Interpreter (
    interpret
  , interpret'
  ) where

import           AdderService.Adder        (AdderT)
import           AdderService.Functors     (AdderF (..))

import           Control.Monad.Reader      (ReaderT, ask, runReaderT)
import           Control.Monad.State       (StateT, evalStateT, get, put)
import           Control.Monad.Trans.Class (MonadTrans, lift)
import           Control.Monad.Trans.Free  (FreeF (..), FreeT (..), runFreeT)

type Limit = Int
type Count = Int

interpret :: Monad m => Limit -> Count -> AdderT m r -> m r
interpret limit count a = do
  mr <- runFreeT a
  case mr of
   Pure r -> return r
   Free (Add x k) -> do
     let count' = x + count
     let test = count' <= limit
     let next = if test then count' else count
     interpret limit next (k test)
   Free (Clear k) ->
     interpret limit 0 k
   Free (Total k) ->
     interpret limit count (k count)

step :: Monad m => AdderF (ReaderT Int (StateT Int m) r) -> ReaderT Int (StateT Int m) r
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

interpret' :: Monad m => Limit -> Count -> AdderT m r -> m r
interpret' limit count = flip evalStateT count .
                         flip runReaderT limit .
                         iterTTM step
