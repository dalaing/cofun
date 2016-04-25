{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances     #-}
-- |
module Util.Coproduct.Old (
  ) where

import           Util.Pairing (Pairing (..))

-- | The sum of two type constructors.
-- For the purposes of this package, it will always be used as a sum of 'Functor's.
data SumF f g x
  = InL (f x) -- ^ The first component of the sum
  | InR (g x) -- ^ The second component of the sum

-- | A type synonym for 'SumF's.
type f :+: g = SumF f g

instance (Functor f, Functor g) => Functor (SumF f g) where
  fmap f (InL x) = InL (fmap f x)
  fmap f (InR x) = InR (fmap f x)

-- | Used to indicate that 'sub' is contained somewhere within the nested sum 'sup',
-- and allows for values of 'sub a' to be injected into a 'sup a'.
--
-- This allows us to work in the context 'f :<: g' where we are working with 'f' but
-- want to generalize to any 'g' that contains 'f'.
class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub a -> sup a

instance Functor f => f :<: f where
  inj = id

instance (Functor f, Functor g, Functor h, f :<: g) => f :<: (SumF h g) where
  inj = InR . inj

-- | A product of two type constructors.
-- For the purposes of this package, it will always be used as a product of 'Functor's.
data ProductF f g x = (f x) :*: (g x)

-- | A type synonym for 'ProductF's.
type f :*: g = ProductF f g

instance (Functor f,Functor g) => Functor (ProductF f g) where
  fmap f (g :*: h) = (fmap f g) :*: (fmap f h)

instance (Pairing f f',Pairing g g') => Pairing (f :+: g) (f' :*: g') where
  pair p (InL x) (a :*: _) = pair p x a
  pair p (InR x) (_ :*: b) = pair p x b

instance (Pairing f f',Pairing g g') => Pairing (f :*: g) (f' :+: g') where
  pair p (a :*: _) (InL x) = pair p a x
  pair p (_ :*: b) (InR x) = pair p b x
