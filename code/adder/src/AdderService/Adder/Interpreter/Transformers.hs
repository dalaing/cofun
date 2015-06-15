module AdderService.Adder.Interpreter.Transformers (
    interpret
  ) where

import           AdderService.Adder        (AdderT)
import           AdderService.Functors     (AdderF (..))

import           Control.Monad.Reader      (ReaderT, ask, runReaderT)
import           Control.Monad.State       (StateT, evalStateT, get, put)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Free  (FreeF (..), runFreeT)

type Limit = Int
type Count = Int

type Base m = ReaderT Limit (StateT Count m)

interpret' :: Monad m => AdderT (Base m) r -> Base m r
interpret' a = do
  mr <- runFreeT a
  case mr of
    Pure r -> return r
    Free (Add x k) -> do
      limit <- ask
      count <- lift get
      let count' = x + count
      let test = count' <= limit
      let next = if test then count' else count
      lift . put $ next
      interpret' (k test)
    Free (Clear k) -> do
      lift . put $ 0
      interpret' k
    Free (Total k) -> do
      count <- lift get
      interpret' (k count)

runBase :: Monad m => Limit -> Count -> Base m r -> m r
runBase limit count =
  flip evalStateT count .
  flip runReaderT limit

interpret :: Monad m => Limit -> Count -> AdderT (Base m) r -> m r
interpret limit count =
  runBase limit count .
  interpret'
