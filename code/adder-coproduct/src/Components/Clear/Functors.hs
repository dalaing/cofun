{-# LANGUAGE MultiParamTypeClasses #-}
module Components.Clear.Functors (
    ClearF(..)
  , CoClearF(..)
  ) where

import Components.Console (Console(..))

import           Util.Pairing (Pairing (..))

import Text.Parser.Char

import Control.Monad (void)

data ClearF k = Clear k

instance Functor ClearF where
  fmap f (Clear k) = Clear (f k)

instance Console ClearF where
    prompt _ = ["clear"]
    parser = do
        void $ string "clear"
        return $ Clear ()

data CoClearF k = CoClear k

instance Functor CoClearF where
  fmap f (CoClear k) = CoClear (f k)

instance Pairing CoClearF ClearF where
    pair f (CoClear c) (Clear k) = f c k
