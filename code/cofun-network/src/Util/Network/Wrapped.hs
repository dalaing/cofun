{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Util.Network.Wrapped where

import Util.Coproduct
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
        f (NetworkClientF (req, g)) = NetworkClientF (Tag req, (\(Tag y) -> g y))

instance (Monad m, KnownNat n, ToNetworkInterpreter i m, Binary (InterpreterReq i), Binary (InterpreterRes i)) => ToNetworkInterpreter (TagNat1 n i) m where
    type InterpreterReq (TagNat1 n i) = TagNat n (InterpreterReq i)
    type InterpreterRes (TagNat1 n i) = TagNat n (InterpreterRes i)
    toNetworkInterpreter = f . toNetworkInterpreter . untag1
      where
        f :: NetworkInterpreterF (InterpreterReq i) (InterpreterRes i) m k -> NetworkInterpreterF (TagNat n (InterpreterReq i)) (TagNat n (InterpreterRes i)) m k
        f (NetworkInterpreterF g) = NetworkInterpreterF $ fmap (\(x,y) -> (Tag x, y)) . g . untag


data BinarySum (l :: [*]) where
  BAdd :: Binary h => h -> BinarySum (h ': t)
  BNext :: BinarySum t -> BinarySum (h ': t) 

class BinaryHelper (l :: [*]) where
  binLength :: Proxy l -> Word8
  putHelper :: Word8 -> BinarySum l -> Put
  getHelper :: Word8 -> Word8 -> Get (BinarySum l)

instance Binary h => BinaryHelper (h ': '[]) where
  binLength _ = 1
  putHelper n (BAdd h) = put n >> put h
  getHelper 1 1 = BAdd <$> get 
  getHelper _ _ = empty

instance (Binary h, BinaryHelper (i ': j)) => BinaryHelper (h ': (i ': j)) where
  binLength _ = 1 + binLength (Proxy :: Proxy (i  ': j))

  putHelper n (BAdd h) = put n >> put h
  putHelper n (BNext t) = putHelper (n - 1) t

  getHelper l n
    | l == n = BAdd <$> get
    | otherwise = BNext <$> getHelper (l - 1) n

instance BinaryHelper l => Binary (BinarySum l) where
    put = putHelper (binLength (Proxy :: Proxy l))
    get = get >>= getHelper (binLength (Proxy :: Proxy l))

class ClientSumHelper m (l :: [(* -> *)]) where
  type ClientReqList l :: [*]
  type ClientResList l :: [*]

instance ClientSumHelper m '[] where
  type ClientReqList '[] = '[]
  type ClientResList '[] = '[]

instance (ToNetworkClient h m, ClientSumHelper m t) => ClientSumHelper m (h ': t) where
  type ClientReqList (h ': t) = (ClientReq h) ': (ClientReqList t)
  type ClientResList (h ': t) = (ClientRes h) ': (ClientResList t)

instance (ToNetworkClient h m) => ToNetworkClient (Sum (h ': '[])) m where
  type ClientReq (Sum (h ': '[])) = BinarySum (ClientReqList (h ': '[]))
  type ClientRes (Sum (h ': '[])) = BinarySum (ClientResList (h ': '[]))

  toNetworkClient (SAdd h) =  _

instance (ToNetworkClient h m, ToNetworkClient (Sum (i ': j)) m) => ToNetworkClient (Sum (h ': (i ': j))) m where
  type ClientReq (Sum (h ': (i ': j))) = BinarySum (ClientReqList (h ': (i ': j)))
  type ClientRes (Sum (h ': (i ': j))) = BinarySum (ClientResList (h ': (i ': j)))

  toNetworkClient (SAdd h) =  _
  toNetworkClient (SNext t) = _

-- tonetworkclient of sumf, where each component of the sum has
-- a tonetworkclient instance

-- tonetworkinterpreter of product, where each component of the product has
-- a tonetworkinterpreter instance

-- we want to do this for all combinations of sum/product and client/interpreter
-- probably weave a proxy nat through all of this, kick it off with proxy 0

-- given a sum of tonetworkclients, build the request and response types
-- these should be a sum of binaries (after wrapping)
-- toNetworkClient should match up the sums on the inputs to the same
-- branches of the sums on the outputs, calling the inner toNetworkClient
-- as they go

