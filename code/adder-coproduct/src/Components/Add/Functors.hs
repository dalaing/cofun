{-# LANGUAGE MultiParamTypeClasses #-}
module Components.Add.Functors (
      AddF(..)
    , CoAddF(..)
    ) where

import           Util.Pairing (Pairing (..))

data AddF k = Add Int (Bool -> k)

instance Functor AddF where
  fmap f (Add x k) = Add x (f . k)

data CoAddF k = CoAdd (Int -> (Bool, k))

instance Functor CoAddF where
  fmap f (CoAdd a) = CoAdd (fmap (fmap f) a)

instance Pairing CoAddF AddF where
    pair f (CoAdd a) (Add x k) = pair f (a x) k

