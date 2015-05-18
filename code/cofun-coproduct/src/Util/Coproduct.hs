{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
module Util.Coproduct (
      (:+:)(..)
    , (:<:)(..)
    , (:*:)(..)
    , (*:*)
    ) where

import           Util.Pairing        (Pairing (..))

import           Control.Applicative (liftA2)

data (f :+: g) x = InL (f x) | InR (g x)

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (InL x) = InL (fmap f x)
  fmap f (InR x) = InR (fmap f x)

class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub a -> sup a

instance Functor f => f :<: f where
    inj = id

instance (Functor f, Functor g) => f :<: (f :+: g) where
    inj = InL

instance (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
    inj = InR . inj

data (f :*: g) x = f x :*: g x

instance (Functor f, Functor g) => Functor (f :*: g) where
  fmap h (f :*: g) = fmap h f :*: fmap h g

(*:*) :: (Functor f, Functor g) => (a -> f a) -> (a -> g a) -> a -> (f :*: g) a
(*:*) = liftA2 (:*:)

instance (Pairing f f', Pairing g g') => Pairing (f :+: g) (f' :*: g') where
  pair p (InL x) (a :*: _) = pair p x a
  pair p (InR x) (_ :*: b) = pair p x b

instance (Pairing f f', Pairing g g') => Pairing (f :*: g) (f' :+: g') where
  pair p (a :*: _) (InL x) = pair p a x
  pair p (_ :*: b) (InR x) = pair p b x

