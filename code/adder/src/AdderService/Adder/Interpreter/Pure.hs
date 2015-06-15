module AdderService.Adder.Interpreter.Pure (
    interpret
  ) where

import           AdderService.Adder       (AdderT)
import           AdderService.Functors    (AdderF (..))

import           Control.Monad.Trans.Free (FreeF (..), runFreeT)

type Limit = Int
type Count = Int

interpret :: Monad m => Limit -> Count -> AdderT m r -> m r
interpret limit count a = do
  mr <- runFreeT a
  case mr of
    Pure r -> return r
    Free (Add x k) ->
      let
        count' = x + count
        test = count' <= limit
        next = if test then count' else count
      in
        interpret limit next (k test)
    Free (Clear k) ->
      interpret limit 0 k
    Free (Total k) ->
      interpret limit count (k count)
