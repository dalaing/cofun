module AdderService.Adder (
    AdderT
  , add
  , clear
  , total
  ) where

import           AdderService.Functors    (AdderF (..))

import           Control.Monad.Trans.Free (FreeT, liftF)

type AdderT = FreeT AdderF

add :: Monad m => Int -> AdderT m Bool
add x = liftF $ Add x id

clear :: Monad m => AdderT m ()
clear = liftF $ Clear ()

total :: Monad m => AdderT m Int
total = liftF $ Total id
