{-# LANGUAGE MultiParamTypeClasses #-}
module Components.Add.Functors (
      AddF(..)
    , CoAddF(..)
    ) where

import Components.Console (Console(..))

import           Util.Pairing (Pairing (..))


import Data.Functor ((<$))

import Text.Parser.Char
import Text.Parser.Combinators

data AddF k = Add Int (Bool -> k)

instance Functor AddF where
  fmap f (Add x k) = Add x (f . k)

instance Console AddF where
    prompt _ = ["add (int)"]
    parser = 
      string "add" >> 
      space >> 
      many digit >>= \xs -> 
        return $ Add (read xs) (const ())

data CoAddF k = CoAdd (Int -> (Bool, k))

instance Functor CoAddF where
  fmap f (CoAdd a) = CoAdd (fmap (fmap f) a)

results :: Functor f => CoAddF (f a) -> CoAddF (f (IO ()))
results (CoAdd f) = CoAdd $ \x ->
  let 
    (b, k) = f x
  in 
    (b, putStrLn ("add result: " ++ show b) <$ k)

instance Pairing CoAddF AddF where
    pair f (CoAdd a) (Add x k) = pair f (a x) k

