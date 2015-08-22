{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Util.Coproduct where

import Util.Pairing

import Control.Applicative
import Data.Proxy
import Data.Type.Equality

class NotIn (x :: (* -> *)) (l :: [(* -> *)])
instance NotIn x '[]
instance (NotIn x t, (x == h) ~ 'False) => NotIn x (h ': t)

data ProductF (l :: [(* -> *)]) (a :: *) where
  POne  :: Functor h => h a -> ProductF (h ': '[]) a
  PAdd  :: (Functor h, NotIn h t) => h a -> ProductF t a -> ProductF (h ': t) a

instance Functor (ProductF l) where
  fmap f (POne h) = POne (fmap f h)
  fmap f (PAdd h t) = PAdd (fmap f h) (fmap f t)

class PAppend (t :: [(* -> *)]) where
    type I t :: * -> * 
    (*:*) :: (Functor h, NotIn h t) => (a -> h a) -> (a -> (I t) a) -> a -> ProductF (h ': t) a

instance Functor h => PAppend (h ': '[]) where
    type I (h ': '[]) = h
    (*:*) f g a = PAdd (f a) (POne (g a))

instance PAppend (h ': (i ': j)) where
    type I (h ': (i ': j)) = ProductF (h ': (i ': j))
    (*:*) f g a = PAdd (f a) (g a)

data SumF (l :: [(* -> *)]) (a :: *) where
  SNext  :: (Functor h, NotIn h t) => SumF t a -> SumF (h ': t) a
  SAdd   :: (Functor h, NotIn h t) => h a -> SumF (h ': t) a

instance Functor (SumF l) where
  fmap f (SNext t) = SNext (fmap f t)
  fmap f (SAdd h) = SAdd (fmap f h)

class Contains (x :: (* -> *)) (xs :: [(* -> *)]) where
  inj :: Functor x => x a -> SumF xs a

instance (NotIn x t) => Contains x (x ': t) where
  inj = SAdd

instance (Functor h, NotIn h t, Contains x t) => Contains x (h ': t) where
  inj = SNext . inj

instance (Pairing h1 h2) => Pairing (ProductF (h1 ': '[])) (SumF (h2 ': '[])) where
  pair f (POne ph) (SAdd sh) = pair f ph sh

instance (Pairing h1 h2, Pairing (ProductF (t1 ': u1)) (SumF (t2 ': u2))) => Pairing (ProductF (h1 ': (t1 ': u1))) (SumF (h2 ': (t2 ': u2))) where
  pair f (PAdd ph _) (SAdd sh) = pair f ph sh
  pair f (PAdd _ pt) (SNext st) = pair f pt st

instance (PairingM h1 h2 m) => PairingM (ProductF (h1 ': '[])) (SumF (h2 ': '[])) m where
  pairM f (POne ph) (SAdd sh) = pairM f ph sh

instance (PairingM h1 h2 m, PairingM (ProductF (t1 ': u1)) (SumF (t2 ': u2)) m) => PairingM (ProductF (h1 ': (t1 ': u1))) (SumF (h2 ': (t2 ': u2))) m where
  pairM f (PAdd ph _) (SAdd sh) = pairM f ph sh
  pairM f (PAdd _ pt) (SNext st) = pairM f pt st
