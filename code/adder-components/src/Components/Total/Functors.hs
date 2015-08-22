{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Components.Total.Functors (
    TotalF(..)
  , CoTotalF(..)
  ) where

import Components.Total.Packets (TotalReq(..), TotalRes(..))

import           Util.Console (ConsoleClient (..),
                                     ConsoleInterpreter (..))
import           Util.Network      (ToNetworkClient (..), ToNetworkInterpreter (..))
import           Util.Network.Functors   (NetworkClientF (..), NetworkInterpreterF (..))
import           Util.Pairing       (Pairing (..))

import           Control.Monad      (void)

import           Text.Parser.Char

data TotalF k = Total (Int -> k)

instance Functor TotalF where
  fmap f (Total k) = Total (f . k)

data CoTotalF k = CoTotal (Int, k)

instance Functor CoTotalF where
    fmap f (CoTotal t) = CoTotal (fmap f t)

instance Pairing CoTotalF TotalF where
    pair f (CoTotal t) (Total k) = pair f t k

instance ConsoleClient TotalF where
  prompt _ = ["total"]
  parser = do
    void $ string "total"
    return $ Total (const ())

instance ConsoleInterpreter CoTotalF where
  addResultLogging (CoTotal (i, k)) = CoTotal (i, putStrLn ("total result: " ++ show i) <$ k)

instance Monad m => ToNetworkClient TotalF m where
  type ClientReq TotalF = TotalReq
  type ClientRes TotalF = TotalRes
  toNetworkClient (Total f) = NetworkClientF (TotalReq, return (\(TotalRes i) -> f i))

instance Monad m => ToNetworkInterpreter CoTotalF m where
  type InterpreterReq CoTotalF = TotalReq
  type InterpreterRes CoTotalF = TotalRes
  toNetworkInterpreter (CoTotal (i, k)) = NetworkInterpreterF $
    \(TotalReq) -> return (TotalRes i, k)

