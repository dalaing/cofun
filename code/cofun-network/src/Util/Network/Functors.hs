{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Util.Network.Functors (
    NetworkClientF(..)
  , NetworkInterpreterF(..)
  ) where

import           Util.Pairing

data NetworkClientF req res m k = NetworkClientF (req, m (res -> k))

instance Functor m => Functor (NetworkClientF req res m)  where
  fmap f (NetworkClientF k) = NetworkClientF (fmap (fmap (fmap f)) k)

data NetworkInterpreterF req res m k = NetworkInterpreterF (req -> m (res, k))

instance Functor m => Functor (NetworkInterpreterF req res m) where
  fmap f (NetworkInterpreterF k) = NetworkInterpreterF (fmap (fmap (fmap f)) k)

instance Monad m => PairingM (NetworkInterpreterF req res m) (NetworkClientF req res m) m where
    pairM p (NetworkInterpreterF fi) (NetworkClientF (rq, fc)) = do
      (rs, ki) <- fi rq 
      f <- fc
      p ki (f rs)

