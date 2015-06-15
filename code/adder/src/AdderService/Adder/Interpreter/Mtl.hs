{-# LANGUAGE FlexibleContexts #-}
module AdderService.Adder.Interpreter.Mtl (
    interpret
  ) where

import           AdderService.Adder    (AdderT)
import           AdderService.Functors (AdderF (..))

import           Control.Monad.Trans.Free (FreeF (..), runFreeT)

import Control.Monad.Reader(ReaderT, runReaderT)
import Control.Monad.Reader.Class (MonadReader, ask)
import Control.Monad.State(StateT, evalStateT)
import Control.Monad.State.Class (MonadState, get, put)

type Limit = Int
type Count = Int

interpret' :: (MonadReader Limit m, MonadState Count m) => AdderT m r -> m r
interpret' a = do
  mr <- runFreeT a
  case mr of
    Pure r -> return r
    Free (Add x k) -> do
      limit <- ask
      count <- get
      let count' = x + count
      let test = count' <= limit
      let next = if test then count' else count
      put next
      interpret' (k test)
    Free (Clear k) -> do
      put 0
      interpret' k
    Free (Total k) -> do
      count <- get
      interpret' (k count)

type Base m = ReaderT Limit (StateT Count m)

interpret :: (Monad m) => Limit -> Count -> AdderT (Base m) r -> m r
interpret limit count =
  flip evalStateT count .
  flip runReaderT limit .
  interpret'
