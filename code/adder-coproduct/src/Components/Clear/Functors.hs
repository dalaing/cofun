{-# LANGUAGE MultiParamTypeClasses #-}
module Components.Clear.Functors (
    ClearF(..)
  , CoClearF(..)
  ) where

import           Util.Console (ConsoleClient (..),
                                     ConsoleInterpreter (..))
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

instance ConsoleInterpreter CoClearF where
  addResultLogging (CoClear k) = CoClear (return () <$ k)

