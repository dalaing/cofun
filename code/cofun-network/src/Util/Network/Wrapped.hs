{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Util.Network.Wrapped where

import Util.Network
import Util.Network.Functors

import Control.Applicative
import Data.Binary

import GHC.TypeLits
import Data.Proxy

newtype TagNat (n :: Nat) p = Tag { untag :: p }
newtype TagNat1 (n :: Nat) p k = Tag1 { untag1 :: p k }

-- TODO functors for theses
-- TODO pairing for TagNat1

instance (KnownNat n, Binary p) => Binary (TagNat n p) where
    put (Tag p) = put (natVal (Proxy :: Proxy n)) >> put p
    get = do
        x <- get :: Get Integer
        if x == natVal (Proxy :: Proxy n)
           then Tag <$> get
           else empty

instance (Monad m, KnownNat n, ToNetworkClient c m, Binary (ClientReq c), Binary (ClientRes c)) => ToNetworkClient (TagNat1 n c) m where
    type ClientReq (TagNat1 n c) = TagNat n (ClientReq c)
    type ClientRes (TagNat1 n c) = TagNat n (ClientRes c)
    toNetworkClient = f . toNetworkClient . untag1
      where
        f :: NetworkClientF (ClientReq c) (ClientRes c) m k -> NetworkClientF (TagNat n (ClientReq c)) (TagNat n (ClientRes c)) m k
        f (NetworkClientF (req, g)) = NetworkClientF (Tag req, (\h (Tag y) -> h y) <$> g)

instance (Monad m, KnownNat n, ToNetworkInterpreter i m, Binary (InterpreterReq i), Binary (InterpreterRes i)) => ToNetworkInterpreter (TagNat1 n i) m where
    type InterpreterReq (TagNat1 n i) = TagNat n (InterpreterReq i)
    type InterpreterRes (TagNat1 n i) = TagNat n (InterpreterRes i)
    toNetworkInterpreter = f . toNetworkInterpreter . untag1
      where
        f :: NetworkInterpreterF (InterpreterReq i) (InterpreterRes i) m k -> NetworkInterpreterF (TagNat n (InterpreterReq i)) (TagNat n (InterpreterRes i)) m k
        f (NetworkInterpreterF g) = NetworkInterpreterF $ fmap (\(x,y) -> (Tag x, y)) . g . untag

-- we want to take a list of functors (possibly already in a SumF)
-- and build the sum of client functors (ie call toclient on each of them)
-- we also want to bring the data across

-- we want to do this for all combinations of sum/product and client/interpreter
-- probably weave a proxy nat through all of this, kick it off with proxy 0

