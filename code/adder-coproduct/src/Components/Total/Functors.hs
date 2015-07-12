{-# LANGUAGE MultiParamTypeClasses #-}
module Components.Total.Functors (
    TotalF(..)
  , CoTotalF(..)
  ) where

import           Components.Console (ConsoleClient (..),
                                     ConsoleInterpreter (..))

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
