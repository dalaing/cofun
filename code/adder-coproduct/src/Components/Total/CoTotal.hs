{-# LANGUAGE FlexibleContexts      #-}
module Components.Total.CoTotal (
    coTotal
  ) where

import           Components.Total.Functors   (CoTotalF (..))

import           Control.Comonad.Store.Class (ComonadStore, pos)

coTotal :: ComonadStore Int w => w a -> CoTotalF (w a)
coTotal w = CoTotal (pos w, w)

