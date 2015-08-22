{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Components.Add.Functors (
      AddF(..)
    , CoAddF(..)
    ) where

import Components.Add.Packets (AddReq(..), AddRes(..))

import           Util.Console      (ConsoleClient (..),
                                          ConsoleInterpreter (..))
import           Util.Network      (ToNetworkClient (..), ToNetworkInterpreter (..))
import           Util.Network.Functors   (NetworkClientF (..), NetworkInterpreterF (..))
import           Util.Pairing            (Pairing (..))

import           Text.Parser.Char
import           Text.Parser.Combinators

data AddF k = Add Int (Bool -> k)

instance Functor AddF where
  fmap f (Add x k) = Add x (f . k)

data CoAddF k = CoAdd (Int -> (Bool, k))

instance Functor CoAddF where
  fmap f (CoAdd a) = CoAdd (fmap (fmap f) a)

instance Pairing CoAddF AddF where
    pair f (CoAdd a) (Add x k) = pair f (a x) k

instance ConsoleClient AddF where
    prompt _ = ["add (int)"]
    parser =
      string "add" >>
      space >>
      many digit >>= \xs ->
        return $ Add (read xs) (const ())

instance ConsoleInterpreter CoAddF where
  addResultLogging (CoAdd f) = CoAdd (fmap (\(b, k) -> (b, putStrLn ("add result: " ++ show b) <$ k)) f)

instance Monad m => ToNetworkClient AddF m where
  type ClientReq AddF = AddReq
  type ClientRes AddF = AddRes
  toNetworkClient (Add x f) = NetworkClientF (AddReq x, return (\(AddRes b) -> f b))

instance Monad m => ToNetworkInterpreter CoAddF m where
  type InterpreterReq CoAddF = AddReq
  type InterpreterRes CoAddF = AddRes
  toNetworkInterpreter (CoAdd f) = NetworkInterpreterF $
    \(AddReq x) -> case f x of
      (b, k) -> return (AddRes b, k)

