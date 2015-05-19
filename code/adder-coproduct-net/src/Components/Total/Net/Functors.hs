{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Components.Total.Net.Functors (
  ) where

import Components.Errors (NetError(..))

import Components.Total.Net.Request (TotalReq(..))
import Components.Total.Net.Response (TotalRes(..))

import Components.Total.Functors (TotalF(..), CoTotalF(..))

import Util.Pairing (PairingM(..))

import Control.Monad.Except (MonadError, throwError)

data TotalClientF m k = TotalClientF TotalReq (m (Either NetError TotalRes -> k))

instance Functor m => Functor (TotalClientF m) where
  fmap f (TotalClientF r k) = TotalClientF r (fmap (fmap f) k)

data TotalServerF m k = TotalServerF (TotalReq -> m (Either NetError TotalRes, k))

instance Functor m => Functor (TotalServerF m) where
  fmap f (TotalServerF k) = TotalServerF (fmap (fmap (fmap f)) k)

instance (Functor m, Monad m) => PairingM (TotalServerF m) (TotalClientF m) m where
  pairM f (TotalServerF k) (TotalClientF req j) = do
    (res, a) <- k req
    j' <- j
    f a (j' res)

instance (Functor m, MonadError NetError m) => PairingM (TotalServerF m) TotalF m where
  pairM f (TotalServerF c) (Total k) = do
    r <- c TotalReq
    case r of
     (Left l, _) -> throwError l
     (Right (TotalRes b), j) -> f j (k b)

instance (Functor m, MonadError NetError m) => PairingM CoTotalF (TotalClientF m) m where
  pairM f (CoTotal t) (TotalClientF TotalReq k) = do
    r <- k
    f (snd t) (r (Right (TotalRes (fst t))))
