{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Components.Add.Net.Functors (
  ) where

import Components.Errors (NetError(..))

import Components.Add.Net.Request (AddReq(..))
import Components.Add.Net.Response (AddRes(..))

import Components.Add.Functors (AddF(..), CoAddF(..))

import Util.Pairing (PairingM(..))

import Control.Monad.Except (MonadError, throwError)

data AddClientF m k = AddClientF AddReq (m (Either NetError AddRes -> k))

instance Functor m => Functor (AddClientF m) where
  fmap f (AddClientF r k) = AddClientF r (fmap (fmap f) k)

data AddServerF m k = AddServerF (AddReq -> m (Either NetError AddRes, k))

instance Functor m => Functor (AddServerF m) where
  fmap f (AddServerF k) = AddServerF (fmap (fmap (fmap f)) k)

instance (Functor m, Monad m) => PairingM (AddServerF m) (AddClientF m) m where
  pairM f (AddServerF k) (AddClientF req j) = do
    (res, a) <- k req
    j' <- j
    f a (j' res)

instance (Functor m, MonadError NetError m) => PairingM (AddServerF m) AddF m where
  pairM f (AddServerF c) (Add x k) = do
    r <- c (AddReq x)
    case r of
     (Left l, _) -> throwError l
     (Right (AddRes b), j) -> f j (k b)

instance (Functor m, MonadError NetError m) => PairingM CoAddF (AddClientF m) m where
  pairM f (CoAdd a) (AddClientF (AddReq x) k) = do
    r <- k
    f (snd (a x)) (r (Right (AddRes (fst (a x)))))
