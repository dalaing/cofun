{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module Util.Pairing (
      Pairing(..)
    , pairEffect
    , PairingM(..)
    , pairEffectM
    ) where

import           Control.Comonad              (Comonad, extract)
import           Control.Comonad.Trans.Cofree (CofreeT, unwrap)
import           Control.Monad.Trans.Free     (FreeF (..), FreeT, runFreeT)
import           Data.Functor.Identity        (Identity (..))

class Pairing f g | f -> g, g -> f where
  pair :: (a -> b -> r) -> f a -> g b -> r

instance Pairing Identity Identity where
  pair f (Identity a) (Identity b) = f a b

instance Pairing ((->) a) ((,) a) where
  pair p f = uncurry (p . f)

instance Pairing ((,) a) ((->) a) where
  pair p f g = p (snd f) (g (fst f))

pairEffect :: (Pairing f g, Functor f, Functor g, Comonad w, Monad m)
           => (a -> b -> r) -> CofreeT f w (m a) -> FreeT g m b -> m r
pairEffect p s c = do
  a  <- extract s
  mb <- runFreeT c
  case mb of
    Pure x -> return $ p a x
    Free gs -> pair (pairEffect p) (unwrap s) gs

runPairWM :: (Pairing f g, Functor f, Functor g, Comonad w, Monad m)
           => (a -> b -> r) -> CofreeT f w (m a) -> FreeT g m b -> m r
runPairWM p s c = do
  a  <- extract s
  mb <- runFreeT c
  case mb of
    Pure x -> return $ p a x
    Free gs -> pair (runPairWM p) (unwrap s) gs

runPairMW :: (Pairing f g, Functor f, Functor g, Comonad w, Monad m)
           => (a -> b -> r) -> CofreeT f w (m a) -> FreeT g m b -> m r
runPairMW p s c = do
  mb <- runFreeT c
  a  <- extract s
  case mb of
    Pure x -> return $ p a x
    Free gs -> pair (runPairMW p) (unwrap s) gs

-- not 100% sure about this, some work with servant should sort out the
-- shape of it
class PairingM f g m | f -> g, g -> f where
  pairM :: (a -> b -> m r) -> f a -> g b -> m r

pairEffectM :: (Functor (f m), PairingM (f m) (g m) m,  Comonad w, Monad m)
            => (a -> b -> m r) -> CofreeT (f m) w (m a) -> FreeT (g m) m b -> m r
pairEffectM p s c = do
  a <- extract s
  mb <- runFreeT c
  case mb of
    Pure x -> p a x
    Free gs -> pairM (pairEffectM p) (unwrap s) gs
