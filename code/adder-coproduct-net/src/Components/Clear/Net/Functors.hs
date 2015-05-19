{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Components.Clear.Net.Functors (
  ) where

import Components.Errors (NetError(..))

import Components.Clear.Net.Request (ClearReq(..))
import Components.Clear.Net.Response (ClearRes(..))

import Components.Clear.Functors (ClearF(..), CoClearF(..))

import Util.Pairing (PairingM(..))

import Control.Monad.Except (MonadError, throwError)

data ClearClientF m k = ClearClientF ClearReq (m (Either NetError ClearRes -> k))

instance Functor m => Functor (ClearClientF m) where
  fmap f (ClearClientF r k) = ClearClientF r (fmap (fmap f) k)

data ClearServerF m k = ClearServerF (ClearReq -> m (Either NetError ClearRes, k))

instance Functor m => Functor (ClearServerF m) where
  fmap f (ClearServerF k) = ClearServerF (fmap (fmap (fmap f)) k)

instance (Functor m, Monad m) => PairingM (ClearServerF m) (ClearClientF m) m where
  pairM f (ClearServerF k) (ClearClientF req j) = do
    (res, a) <- k req
    j' <- j
    f a (j' res)

instance (Functor m, MonadError NetError m) => PairingM (ClearServerF m) ClearF m where
  pairM f (ClearServerF c) (Clear k) = do
    r <- c ClearReq
    case r of
     (Left l, _) -> throwError l
     (Right ClearRes, j) -> f j k 

instance (Functor m, MonadError NetError m) => PairingM CoClearF (ClearClientF m) m where
  pairM f (CoClear c) (ClearClientF ClearReq k) = do
    r <- k
    f c (r (Right ClearRes))

