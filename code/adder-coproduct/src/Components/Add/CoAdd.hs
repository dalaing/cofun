{-# LANGUAGE FlexibleContexts      #-}
module Components.Add.CoAdd (
    coAdd
  ) where

import           Components.Add.Functors     (CoAddF (..))

import           Control.Comonad.Env.Class   (ComonadEnv, ask)
import           Control.Comonad.Store.Class (ComonadStore, pos, seek)

coAdd :: (ComonadEnv Int w, ComonadStore Int w) => w a -> CoAddF (w a)
coAdd w = CoAdd $ \x ->
  let
    count = pos w
    limit = ask w
    count' = count + x
    test = count' <= limit
    next = if test then count' else count
  in
    (test, seek next w)

