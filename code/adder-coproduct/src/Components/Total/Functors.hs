{-# LANGUAGE MultiParamTypeClasses #-}
module Components.Total.Functors (
    TotalF(..)
  , CoTotalF(..)
  ) where

import           Util.Pairing (Pairing (..))

data TotalF k = Total (Int -> k)

instance Functor TotalF where
  fmap f (Total k) = Total (f . k)

data CoTotalF k = CoTotal (Int, k)

instance Functor CoTotalF where
    fmap f (CoTotal t) = CoTotal (fmap f t)

instance Pairing CoTotalF TotalF where
    pair f (CoTotal t) (Total k) = pair f t k
