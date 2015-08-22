{-# LANGUAGE FlexibleContexts      #-}
module Components.Clear.CoClear (
    coClear
  ) where

import           Components.Clear.Functors   (CoClearF (..))

import           Control.Comonad.Store.Class (ComonadStore, seek)

coClear :: ComonadStore Int w => w a -> CoClearF (w a)
coClear = CoClear . seek 0

