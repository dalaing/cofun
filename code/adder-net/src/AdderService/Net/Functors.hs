{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module AdderService.Net.Functors (
    ClientF(..)
  , ServerF(..)
  ) where

import           AdderService.Net.Errors   (NetError (..))
import           AdderService.Net.Request  (Request (..))
import           AdderService.Net.Response (Response (..))

import           AdderService.Functors

import           Util.Pairing              (PairingM (..))

import           Control.Monad.Except      (MonadError, throwError)

data ClientF m k = ClientF Request (m (Either NetError Response -> k))

instance Functor m => Functor (ClientF m) where
  fmap f (ClientF r k) = ClientF r (fmap (fmap f) k)

data ServerF m k = ServerF (Request -> m (Either NetError Response, k))

instance Functor m => Functor (ServerF m) where
  fmap f (ServerF k) = ServerF (fmap (fmap (fmap f)) k)

instance (Functor m, Monad m) => PairingM (ServerF m) (ClientF m) m where
  pairM f (ServerF k) (ClientF req j) = do
    (res, a) <- k req
    j' <- j
    f a (j' res)

instance (Functor m, MonadError NetError m) => PairingM (ServerF m) AdderF m where
  pairM f (ServerF c) (Add x k) = do
    r <- c (AddReq x)
    case r of
     (Left l, _) -> throwError l
     (Right (AddRes b), j) -> f j (k b)
     _ -> throwError UnexpectedResponse
  pairM f (ServerF c) (Clear k) = do
    r <- c ClearReq
    case r of
     (Left l, _) -> throwError l
     (Right ClearRes, j) -> f j k
     _ -> throwError UnexpectedResponse
  pairM f (ServerF c) (Total k) = do
    r <- c TotalReq
    case r of
     (Left l, _) -> throwError l
     (Right (TotalRes t), j) -> f j (k t)
     _ -> throwError UnexpectedResponse

instance (Functor m, MonadError NetError m) => PairingM CoAdderF (ClientF m) m where
  pairM f (CoAdderF a _ _) (ClientF (AddReq x) k) = do
    r <- k
    f (snd (a x)) (r (Right (AddRes (fst (a x)))))
  pairM f (CoAdderF _ c _) (ClientF ClearReq k) = do
    r <- k
    f c (r (Right ClearRes))
  pairM f (CoAdderF _ _ t) (ClientF TotalReq k) = do
    r <- k
    f (snd t) (r (Right (TotalRes (fst t))))
