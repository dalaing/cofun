{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Util.Coproduct.List2 (
    Product(..)
  , PAppend(..)
  , Sum(..)
  , Sum0(..)
  , Contains(..)
  , All
  ) where

import Util.Pairing (Pairing(..), PairingM(..))

import Data.Type.Equality (type (==))
import GHC.Exts (Constraint)

class NotIn (x :: k) (l :: [k])
instance NotIn x '[]
instance (NotIn x t, (x == h) ~ 'False) => NotIn x (h ': t)

type family All (c :: k -> Constraint) (xs :: [k]) :: Constraint where
  All c '[] = ()
  All c (x ': xs) = (c x, All c xs)

type family Reduce (f :: (k -> [k] -> Constraint)) (l :: [k]) :: Constraint where
  Reduce f '[] = ()
  Reduce f (h ': t) = (f h t, Reduce f t)

type Unique (l :: [k]) = Reduce NotIn l

data Product (l :: [(* -> *)]) (a :: *) where
  POne  :: h a -> Product (h ': '[]) a
  PMult  :: h a -> Product t a -> Product (h ': t) a

instance All Functor l => Functor (Product l) where
  fmap f (POne h) = POne (fmap f h)
  fmap f (PMult h t) = PMult (fmap f h) (fmap f t)

class PAppend (t :: [(* -> *)]) where
  type AppendTail t :: * -> *
  (*:*) :: (a -> h a) -> (a -> (AppendTail t) a) -> a -> Product (h ': t) a

instance Functor h => PAppend (h ': '[]) where
  type AppendTail (h ': '[]) = h
  (*:*) f g a = PMult (f a) (POne (g a))

instance PAppend (h ': (i ': j)) where
  type AppendTail (h ': (i ': j)) = Product (h ': (i ': j))
  (*:*) f g a = PMult (f a) (g a)

data Sum0 (l :: [*]) where
  SNext0 :: Sum0 t -> Sum0 (h ': t)
  SAdd0  :: h -> Sum0 (h ': t)

data Sum (l :: [(* -> *)]) (a :: *) where
  SNext :: Sum t a -> Sum (h ': t) a
  SAdd  :: h a -> Sum (h ': t) a

instance All Functor l => Functor (Sum l) where
  fmap f (SNext t) = SNext (fmap f t)
  fmap f (SAdd h) = SAdd (fmap f h)

class Contains (x :: (* -> *)) (xs :: [(* -> *)]) where
  inj :: x a -> Sum xs a

instance Contains x (x ': t) where
  inj = SAdd

instance Contains x t => Contains x (h ': t) where
  inj = SNext . inj

instance (Pairing h1 h2) => Pairing (Product (h1 ': '[])) (Sum (h2 ': '[])) where
  pair f (POne ph) (SAdd sh) = pair f ph sh

instance (Unique (h1 ': t1 ': u1), Unique (h2 ': t2 ': u2), Pairing h1 h2, Pairing (Product (t1 ': u1)) (Sum (t2 ': u2))) => Pairing (Product (h1 ': (t1 ': u1))) (Sum (h2 ': (t2 ': u2))) where
  pair f (PMult ph _) (SAdd sh) = pair f ph sh
  pair f (PMult _ pt) (SNext st) = pair f pt st

instance (PairingM h1 h2 m) => PairingM (Product (h1 ': '[])) (Sum (h2 ': '[])) m where
  pairM f (POne ph) (SAdd sh) = pairM f ph sh

instance (Unique (h1 ': t1 ': u1), Unique (h2 ': t2 ': u2), PairingM h1 h2 m, PairingM (Product (t1 ': u1)) (Sum (t2 ': u2)) m) => PairingM (Product (h1 ': (t1 ': u1))) (Sum (h2 ': (t2 ': u2))) m where
  pairM f (PMult ph _) (SAdd sh) = pairM f ph sh
  pairM f (PMult _ pt) (SNext st) = pairM f pt st

{-

data A a = A a

instance Functor A where
  fmap f (A x) = A (f x)

data B a = B a

instance Functor B where
  fmap f (B x) = B (f x)

instance Pairing A B where
  pair f (A x) (B y) = f x y

data C a = C a

instance Functor C where
  fmap f (C x) = C (f x)

data D a = D a

instance Functor D where
  fmap f (D x) = D (f x)

instance Pairing C D where
  pair f (C x) (D y) = f x y

data E a = E Int

-- test functor for P A C - should work
x1 :: Product [A, C] Int
x1 = (* 2) <$> PMult (A 1) (POne (C 3))

-- test functor for S B D - should work
x2b :: Sum [B, D] Int
x2b = (* 2) <$> SAdd (B 2)

x2d :: Sum [B, D] Int
x2d = (* 2) <$> SNext (SAdd (D 4))

-- test functor for P A E - shouldn't work

-- x3 :: Product [A, E] Int
-- x3 = (* 2) <$> PMult (A 1) (POne (E 3))

-- test functor for S B E - shouldn't work

-- x4b :: Sum [B, E] Int
-- x4b = (* 2) <$> SAdd (B 2)

-- x4e :: Sum [B, E] Int
-- x4e = (* 2) <$> SNext (SAdd (E 4))

-- y4e :: Sum [B, E] Int
-- y4e = fmap (* 2) x4e

pa :: Product '[A] Int
pa = POne (A 1)

sb :: Sum '[B] Int
sb = SAdd (B 2)

sc :: Sum '[C] Int
sc = SAdd (C 2)

-- test pair with P A and S B - should work
pasb :: Int
pasb = pair (+) pa sb

-- test pair with P A and S C - should fail, no pairing
-- pasc :: Int
-- pasc = pair (+) pa sc

-- test pair with P A C and S B D - should work
-- test pair with P A C and S B E - should fail, no pairing
-- test pair with P A E and S B D - should fail, no pairing

pac :: Product '[A, C] Int
pac = PMult (A 1) (POne (C 3))

pae :: Product '[A, E] Int
pae = PMult (A 1) (POne (E 3))

sbd1 :: Sum '[B, D] Int
sbd1 = SAdd (B 2)

sbd2 :: Sum '[B, D] Int
sbd2 = SNext (SAdd (D 4))

sbe1 :: Sum '[B, E] Int
sbe1 = SAdd (B 2)

sbe2 :: Sum '[B, E] Int
sbe2 = SNext (SAdd (E 4))

-- pacsbd1 :: Int
-- pacsbd1 = pair (+) pac sbd1

-- pacsbd2 :: Int
-- pacsbd2 = pair (+) pac sbd2

-- pacsbe1 :: Int
-- pacsbe1 = pair (+) pac sbe1

-- pacsbe2 :: Int
-- pacsbe2 = pair (+) pac sbe2

-- paesbd1 :: Int
-- paesbd1 = pair (+) pae sbd1

-- paesbd2 :: Int
-- paesbd2 = pair (+) pae sbd2

-- test pair with P A C and S B - should fail, different links
-- pacsb :: Int
-- pacsb = pair (+) pac sb

-- test pair with P A A and S B B - should fail, not unique

paa :: Product '[A, A] Int
paa = PMult (A 1) (POne (A 2))

sbb1 :: Sum '[B, B] Int
sbb1 = SAdd (B 1)

sbb2 :: Sum '[B, B] Int
sbb2 = SNext (SAdd (B 2))

-- paasbb1 :: Int
-- paasbb1 = pair (+) paa sbb1

-- paasbb2 :: Int
-- paasbb2 = pair (+) paa sbb2

-- test pair with P A C A and S B D B - should fail, not unique

paca :: Product '[A, C, A] Int
paca = PMult (A 1) (PMult (C 2) (POne (A 3)))

sbdb1 :: Sum '[B, D, B] Int
sbdb1 = SAdd (B 1)

sbdb2 :: Sum '[B, D, B] Int
sbdb2 = SNext (SAdd (D 2))

sbdb3 :: Sum '[B, D, B] Int
sbdb3 = SNext (SNext (SAdd (B 3)))

-- pacasbdb1 :: Int
-- pacasbdb1 = pair (+) paca sbdb1

-- pacasbdb2 :: Int
-- pacasbdb2 = pair (+) paca sbdb2

-- pacasbdb3 :: Int
-- pacasbdb3 = pair (+) paca sbdb3

-}
