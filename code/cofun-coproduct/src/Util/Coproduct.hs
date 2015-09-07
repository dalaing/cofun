{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
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

import GHC.Exts (Constraint)

-- TODO can this work with PolyKinds?
class NotIn (x :: (* -> *)) (l :: [(* -> *)])
instance NotIn x '[]
instance (NotIn x t, (x == h) ~ 'False) => NotIn x (h ': t)

class TFold (f :: ((* -> *) -> [(* -> *)] -> Constraint)) (l :: [(* -> *)])
instance TFold f '[]
instance (f h t, TFold f t) => TFold f (h ': t)

class Unique (l :: [* -> *])
instance (TFold NotIn l) => Unique l

-- TODO Can we generalize Append and Contains?
-- TODO Convert from linked lists to size balanced trees
-- - linearize for pairing? or use some fancy kind of type level fold?
-- type aliases for ProductF = Product Functor?
--
-- use the yorgey homewok version of size balancing for the product
--
-- should be able to use singletons to promote a tree type to the type
-- level for tracking what we have / querying / appending, rather than just
-- using the in-built list


-- for now, just a non-empty list would be an improvement

data Product (l :: [(* -> *)]) (a :: *) where
  POne  :: (Functor h) => h a -> Product (h ': '[]) a
  PMult  :: (Functor h, NotIn h t) => h a -> Product t a -> Product (h ': t) a

instance Functor (Product l) where
  fmap f (POne h) = POne (fmap f h)
  fmap f (PMult h t) = PMult (fmap f h) (fmap f t)

class PAppend (t :: [(* -> *)]) where
  type I t :: * -> * 
  (*:*) :: (Functor h, NotIn h t) => (a -> h a) -> (a -> (I t) a) -> a -> Product (h ': t) a

instance Functor h => PAppend (h ': '[]) where
  type I (h ': '[]) = h
  (*:*) f g a = PMult (f a) (POne (g a))

instance PAppend (h ': (i ': j)) where
  type I (h ': (i ': j)) = Product (h ': (i ': j))
  (*:*) f g a = PMult (f a) (g a)

data Sum (l :: [(* -> *)]) (a :: *) where
  SNext :: (NotIn h t) => Sum t a -> Sum (h ': t) a
  SAdd  :: (Functor h, NotIn h t) => h a -> Sum (h ': t) a

instance Functor (Sum l) where
  fmap f (SNext t) = SNext (fmap f t)
  fmap f (SAdd h) = SAdd (fmap f h)

class Contains (x :: (* -> *)) (xs :: [(* -> *)]) where
  inj :: x a -> Sum xs a

instance (Functor x, NotIn x t) => Contains x (x ': t) where
  inj = SAdd

instance (NotIn h t, Contains x t) => Contains x (h ': t) where
  inj = SNext . inj

instance (Pairing h1 h2) => Pairing (Product (h1 ': '[])) (Sum (h2 ': '[])) where
  pair f (POne ph) (SAdd sh) = pair f ph sh

instance (Pairing h1 h2, Pairing (Product (t1 ': u1)) (Sum (t2 ': u2))) => Pairing (Product (h1 ': (t1 ': u1))) (Sum (h2 ': (t2 ': u2))) where
  pair f (PMult ph _) (SAdd sh) = pair f ph sh
  pair f (PMult _ pt) (SNext st) = pair f pt st

instance (PairingM h1 h2 m) => PairingM (Product (h1 ': '[])) (Sum (h2 ': '[])) m where
  pairM f (POne ph) (SAdd sh) = pairM f ph sh

instance (PairingM h1 h2 m, PairingM (Product (t1 ': u1)) (Sum (t2 ': u2)) m) => PairingM (Product (h1 ': (t1 ': u1))) (Sum (h2 ': (t2 ': u2))) m where
  pairM f (PMult ph _) (SAdd sh) = pairM f ph sh
  pairM f (PMult _ pt) (SNext st) = pairM f pt st
