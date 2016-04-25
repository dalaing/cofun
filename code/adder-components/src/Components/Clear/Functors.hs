{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Components.Clear.Functors (
    ClearF(..)
  , CoClearF(..)
  ) where

import Components.Clear.Packets (ClearReq(..), ClearRes(..))

import           Util.Console (ConsoleClient (..),
                                     ConsoleInterpreter (..))
import           Util.Network      (ToNetworkClient (..), ToNetworkInterpreter (..))
import           Util.Network.Functors   (NetworkClientF (..), NetworkInterpreterF (..))
import           Util.Pairing       (Pairing (..))

import           Text.Parser.Char

import           Control.Monad      (void)

data ClearF k = Clear k

instance Functor ClearF where
  fmap f (Clear k) = Clear (f k)

data CoClearF k = CoClear k

instance Functor CoClearF where
  fmap f (CoClear k) = CoClear (f k)

instance Pairing CoClearF ClearF where
  pair f (CoClear c) (Clear k) = f c k

instance ConsoleClient ClearF where
  prompt _ = ["clear"]
  parser = do
    void $ string "clear"
    return $ Clear ()
  addOutput (Clear k) = Clear k

instance ConsoleInterpreter CoClearF where
  addResultLogging (CoClear k) = CoClear (return () <$ k)

instance Monad m => ToNetworkClient m ClearF where
  type ClientReq ClearF = ClearReq
  type ClientRes ClearF = ClearRes
  toNetworkClient (Clear k) = NetworkClientF (ClearReq, \(ClearRes) -> return k)

instance Monad m => ToNetworkInterpreter m CoClearF where
  type InterpreterReq CoClearF = ClearReq
  type InterpreterRes CoClearF = ClearRes
  toNetworkInterpreter (CoClear k) = NetworkInterpreterF $
    \(ClearReq) -> return (ClearRes, k)

