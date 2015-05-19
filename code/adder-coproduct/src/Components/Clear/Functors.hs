{-# LANGUAGE MultiParamTypeClasses #-}
module Components.Clear.Functors (
    ClearF(..)
  , CoClearF(..)
  ) where

import           Util.Pairing (Pairing (..))

data ClearF k = Clear k

instance Functor ClearF where
  fmap f (Clear k) = Clear (f k)

data CoClearF k = CoClear k

instance Functor CoClearF where
  fmap f (CoClear k) = CoClear (f k)

instance Pairing CoClearF ClearF where
    pair f (CoClear c) (Clear k) = f c k
