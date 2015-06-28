{-# LANGUAGE MultiParamTypeClasses #-}
module Components.Total.Functors (
    TotalF(..)
  , CoTotalF(..)
  ) where

import Components.Console (Console(..))

import           Util.Pairing (Pairing (..))

import Control.Monad (void)

import Text.Parser.Char

data TotalF k = Total (Int -> k)

instance Functor TotalF where
  fmap f (Total k) = Total (f . k)

instance Console TotalF where
    prompt _ = ["total"]
    parser = do 
      void $ string "total" 
      return $ Total (const ())

data CoTotalF k = CoTotal (Int, k)

instance Functor CoTotalF where
    fmap f (CoTotal t) = CoTotal (fmap f t)

instance Pairing CoTotalF TotalF where
    pair f (CoTotal t) (Total k) = pair f t k
