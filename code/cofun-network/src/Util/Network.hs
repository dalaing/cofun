{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Util.Network (
      ToNetworkClient(..)
    , ToNetworkInterpreter(..)
    ) where

import Util.Network.Functors (NetworkClientF, NetworkInterpreterF)

class ToNetworkClient (a :: * -> *) m where
    type ClientReq a
    type ClientRes a
    toNetworkClient :: a k -> NetworkClientF (ClientReq a) (ClientRes a) m k

class ToNetworkInterpreter (a :: * -> *) m where
    type InterpreterReq a
    type InterpreterRes a
    toNetworkInterpreter :: a k -> NetworkInterpreterF (InterpreterReq a) (InterpreterRes a) m k

