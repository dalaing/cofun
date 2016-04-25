{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
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

-- data NetworkClientF req res m k = NetworkClientF (req, res -> m k)
-- data NetworkInterpreterF req res m k = NetworkInterpreterF (req -> m (res, k))

-- class ToNetworkClient (a :: * -> *) m where
--     type ClientReq a
--     type ClientRes a
--     toNetworkClient :: a k -> NetworkClientF (ClientReq a) (ClientRes a) m k

-- class ToNetworkInterpreter (a :: * -> *) m where
--    type InterpreterReq a
--    type InterpreterRes a
--    toNetworkInterpreter :: a k -> NetworkInterpreterF (InterpreterReq a) (InterpreterRes a) m k

class GatherClientReq m (xs :: [* -> *]) where
  type ClientReqSum xs :: [*]

instance (ToNetworkClient m h) => GatherClientReq m (h ': '[]) where
  type ClientReqSum (h ': '[]) = ClientReq h ': '[]

class GatherClientRes m (xs :: [* -> *]) where
  type ClientResSum xs :: [*]

instance (ToNetworkClient m h) => GatherClientRes m (h ': '[]) where
  type ClientResSum (h ': '[]) = ClientRes h ': '[]

instance (Monad m, ToNetworkClient m h) => ToNetworkClient m (Sum (h ': '[])) where
  type ClientReq (Sum (h ': '[])) = Sum0 (ClientReq h ': '[])
  type ClientRes (Sum (h ': '[])) = Sum0 (ClientRes h ': '[])

  toNetworkClient (SAdd h) = NetworkClientF (req', f')
    where
      NetworkClientF (req, f) = toNetworkClient h
      req' = SAdd0 req
      f' (SAdd0 h') = f h'
      f' _ = undefined -- TODO mismatch error

{-
instance (Monad m, ToNetworkClient m h) => ToNetworkClient m (Sum (h ': i ': 't)) where
  type ClientReq (Sum (h ': 'i ': t)) = Sum0 (ClientReqSum l)
  type ClientRes (Sum (h ': 'i ': t)) = Sum0 (ClientResSum l)

  toNetworkClient (SAdd h) =
    let
      NetworkClientF (req, f) = toNetworkClient h
      req' = SAdd0 req
      f' (SAdd0 h') = f h'
      f' _ = undefined -- TODO mismatch error
    in
      NetworkClientF (req', f')

  toNetworkClient (SNext t) =
    let
      NetworkClientF (req, f) = toNetworkClient t
      req' = SNext0 req
      f' (SNext0 t') = f t'
      f' _ = undefined -- TODO mismatch error
    in
      NetworkClientF (req', f')

class GatherInterpreterReq m (xs :: [* -> *]) where
  type InterpreterReqSum xs :: [*]

instance (ToNetworkInterpreter m h) => GatherInterpreterReq m (h ': '[]) where
  type InterpreterReqSum (h ': '[]) = InterpreterReq h ': '[]

class GatherInterpreterRes m (xs :: [* -> *]) where
  type InterpreterResSum xs :: [*]

instance (ToNetworkInterpreter m h) => GatherInterpreterRes m (h ': '[]) where
  type InterpreterResSum (h ': '[]) = InterpreterRes h ': '[]

instance (Monad m, All (ToNetworkInterpreter m) l) => ToNetworkInterpreter m (Sum l) where
  type InterpreterReq (Sum l) = Sum0 (InterpreterReqSum l)
  type InterpreterRes (Sum l) = Sum0 (InterpreterResSum l)

  toNetworkInterpreter (SAdd h) =
    let
      f = toNetworkInterpreter h
      f' (SAdd h') = fmap (\(x,y) -> (SAdd x, y)) (f h')
      f' _ = undefined -- TODO error message
    in
      NetworkInterpreterF f'

  toNetworkInterpreter (SNext t) =
    let
      f = toNetworkInterpreter t
      f' (SNext t') = fmap (\(x,y) -> (SNext x, y)) (f t')
      f' _ = undefined -- TODO error message
    in
      NetworkInterpreterF f'
-}

{-
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

data BinaryList (l :: [*]) where
  BAdd :: Binary h => h -> BinaryList (h ': t)
  BNext :: BinaryList t -> BinaryList (h ': t) 

class BinaryHelper (l :: [*]) where
  binLength :: Proxy l -> Word8
  putHelper :: Word8 -> BinaryList l -> Put
  getHelper :: Word8 -> Word8 -> Get (BinaryList l)

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

instance BinaryHelper l => Binary (BinaryList l) where
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
  type ClientReq (Sum (h ': '[])) = BinaryList (ClientReqList (h ': '[]))
  type ClientRes (Sum (h ': '[])) = BinaryList (ClientResList (h ': '[]))

  toNetworkClient (SAdd h) = _

instance (ToNetworkClient h m, ToNetworkClient (Sum (i ': j)) m) => ToNetworkClient (Sum (h ': (i ': j))) m where
  type ClientReq (Sum (h ': (i ': j))) = BinaryList (ClientReqList (h ': (i ': j)))
  type ClientRes (Sum (h ': (i ': j))) = BinaryList (ClientResList (h ': (i ': j)))

  toNetworkClient (SAdd h) = _
  toNetworkClient (SNext t) = _

class InterpreterProductHelper m (l :: [(* -> *)]) where
  type InterpreterReqList l :: [*]
  type InterpreterResList l :: [*]

instance InterpreterProductHelper m '[] where
  type InterpreterReqList '[] = '[]
  type InterpreterResList '[] = '[]

instance (ToNetworkInterpreter h m, InterpreterProductHelper m t) => InterpreterProductHelper m (h ': t) where
  type InterpreterReqList (h ': t) = (InterpreterReq h) ': (InterpreterReqList t)
  type InterpreterResList (h ': t) = (InterpreterRes h) ': (InterpreterResList t)

instance (ToNetworkInterpreter h m) => ToNetworkInterpreter (Product (h ': '[])) m where
  type InterpreterReq (Product (h ': '[])) = BinaryList (InterpreterReqList (h ': '[]))
  type InterpreterRes (Product (h ': '[])) = BinaryList (InterpreterResList (h ': '[]))

  toNetworkInterpreter (POne h) = _

instance (ToNetworkInterpreter h m, ToNetworkInterpreter (Product (i ': j)) m) => ToNetworkInterpreter (Product (h ': (i ': j))) m where
  type InterpreterReq (Product (h ': (i ': j))) = BinaryList (InterpreterReqList (h ': (i ': j)))
  type InterpreterRes (Product (h ': (i ': j))) = BinaryList (InterpreterResList (h ': (i ': j)))

  toNetworkInterpreter (PMult h t) = _

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

-}
